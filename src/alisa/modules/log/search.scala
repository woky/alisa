package alisa.modules.log

import io.netty.channel._
import io.netty.handler.codec.http._

import io.netty.handler.codec.http.HttpHeaders.Names._
import io.netty.handler.codec.http.HttpHeaders._
import io.netty.handler.codec.http.HttpResponseStatus._
import io.netty.handler.codec.http.HttpVersion._
import alisa.util.Logger
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.util.CharsetUtil._
import scala.collection.JavaConversions._
import io.netty.handler.stream.{ChunkedWriteHandler, ChunkedInput}
import org.apache.lucene.queryparser.classic.ParseException
import scala.Some
import java.text.SimpleDateFormat
import java.util.Date
import io.netty.channel.socket.SocketChannel
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.socket.nio.NioServerSocketChannel
import java.net.InetSocketAddress

final class SearchServer(bindAddr: InetSocketAddress, allowedIds: AllowedIds,
                         lucene: LuceneService) {

	private val bossGroup = new NioEventLoopGroup
	private val workerGroup = new NioEventLoopGroup
	private var channel: Option[Channel] = start

	private def start: Option[Channel] = {
		try {
			val b = new ServerBootstrap
			b.option(ChannelOption.SO_BACKLOG.asInstanceOf[ChannelOption[Any]], 64)
			b.group(bossGroup, workerGroup)
			b.channel(classOf[NioServerSocketChannel])
			b.childHandler(new ChannelInitializer[SocketChannel] {
				def initChannel(ch: SocketChannel) {
					val p = ch.pipeline
					p.addLast("decoder", new HttpRequestDecoder)
					p.addLast("aggregator", new HttpObjectAggregator(65536))
					p.addLast("encoder", new HttpResponseEncoder())
					p.addLast("chunkedWriter", new ChunkedWriteHandler())
					p.addLast("handler", new SearchHandler(allowedIds, lucene))
				}
			})
			val bf = b.bind(bindAddr)
			try {
				Some(bf.sync.channel)
			} catch {
				case e: Exception =>
					bf.channel.close.sync
					throw e
			}
		} catch {
			case e: Exception =>
				bossGroup.shutdownGracefully()
				workerGroup.shutdownGracefully()
				throw e
		}
	}

	def stop {
		synchronized {
			channel match {
				case Some(ch) =>
					ch.close.sync
					bossGroup.shutdownGracefully()
					workerGroup.shutdownGracefully()
					channel = None
				case _ =>
			}
		}
	}
}

private object SearchHandler {

	val OOPS_STR = "¯\\_(ツ)_/¯"
	val CT_TEXT = "text/plain; charset=utf-8"

	val KEY_QUERY = "query"
	val KEY_BYTIME = "sortByTime"
	val KEY_LIMIT = "limit"

	val DEF_QUERY = "*"
	val DEF_BYTIME = true
	val DEF_LIMIT = 50

	val MAX_CHUNK_SIZE = 8192 - 512
}

private final class SearchHandler(allowedIds: AllowedIds, lucene: LuceneService)
		extends SimpleChannelInboundHandler[HttpRequest] with Logger {

	import SearchHandler._

	def channelRead0(ctx: ChannelHandlerContext, msg: HttpRequest) {
		val req = msg.asInstanceOf[HttpRequest]
		if (req.getMethod != HttpMethod.GET) {
			sendError(ctx, UNAUTHORIZED)
			return
		}

		val queryDec = new QueryStringDecoder(req.getUri)
		val path = queryDec.path
		if (path.length != allowedIds.idLen + 1) {
			sendError(ctx, UNAUTHORIZED)
			return
		}

		val luceneChan = allowedIds(path.substring(1)) match {
			case Some(ch) => ch
			case _ =>
				sendError(ctx, UNAUTHORIZED)
				return
		}

		val params = queryDec.parameters.map({
			case (k, v) => k -> v(0)
		})
		val query = params.getOrElse(KEY_QUERY, DEF_QUERY)
		val limit = params.get(KEY_LIMIT).map(_.toInt).getOrElse(DEF_LIMIT)
		val byTime = params.get(KEY_BYTIME).map(_.toBoolean).getOrElse(DEF_BYTIME)

		try {
			val sParams = LuceneSearchParams(limit, byTime)
			val results = lucene.search(query, luceneChan, sParams)

			val resp = new DefaultHttpResponse(HTTP_1_1, OK)
			resp.headers.set(CONTENT_TYPE, CT_TEXT)
			resp.headers.set(TRANSFER_ENCODING, Values.CHUNKED)

			ctx.write(resp)
			ctx.write(new SearchChunkedInput(results.iterator))
			ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT).addListener(ChannelFutureListener.CLOSE)
		} catch {
			case e: ParseException => sendError(ctx, BAD_REQUEST, Some(e.getMessage))
			case e: Exception => logError("lucene.search() failed", e)
		}
	}

	def sendError(ctx: ChannelHandlerContext, status: HttpResponseStatus,
	              optMsg: Option[String] = None) {
		val statusMsg = status.toString + " " + OOPS_STR + "\n"
		val msg = optMsg match {
			case Some(s) => statusMsg + s
			case _ => statusMsg
		}
		val buf = Unpooled.copiedBuffer(msg, UTF_8)
		val resp = new DefaultFullHttpResponse(HTTP_1_1, status, buf)
		resp.headers.set(CONTENT_TYPE, CT_TEXT)
		ctx.write(resp).addListener(ChannelFutureListener.CLOSE)
	}
}

private final class SearchChunkedInput(it: Iterator[LuceneStoredMessage])
		extends ChunkedInput[HttpContent] {

	private val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm")

	private var prevMsg: ByteBuf = _

	def isEndOfInput = !it.hasNext

	def close() {}

	private def formatMsg(msg: LuceneStoredMessage) = {
		val time = dateFormat.format(new Date(msg.stored.time))
		s"$time <${msg.stored.nick}> ${msg.stored.message}\n"
	}

	private def nextMsg =
		if (prevMsg != null) {
			val tmp = prevMsg
			prevMsg = null
			tmp
		} else if (it.hasNext) {
			Unpooled.copiedBuffer(formatMsg(it.next), UTF_8)
		} else {
			null
		}

	def readChunk(ctx: ChannelHandlerContext) =
		nextMsg match {
			case null => null
			case msg =>
				val buf = Unpooled.buffer(SearchHandler.MAX_CHUNK_SIZE)
				def iter(curMsg: ByteBuf) {
					buf.writeBytes(curMsg, Math.min(buf.writableBytes, curMsg.readableBytes))
					if (buf.isWritable)
						nextMsg match {
							case null =>
							case msg => iter(msg)
						}
					else if (curMsg.isReadable)
						prevMsg = curMsg
				}
				iter(msg)
				new DefaultHttpContent(buf)
		}
}
