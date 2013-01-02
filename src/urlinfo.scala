package alisa

import java.util.regex.{Matcher, Pattern}
import javax.net.ssl.{X509TrustManager, TrustManager, SSLContext}
import java.security.cert.X509Certificate
import org.apache.http.conn.ssl.SSLSocketFactory
import org.apache.http.conn.scheme.{PlainSocketFactory, Scheme, SchemeRegistry}
import org.apache.http.impl.conn.PoolingClientConnectionManager
import org.apache.http.params.{HttpConnectionParams, BasicHttpParams}
import org.apache.http.impl.client.DefaultHttpClient
import com.google.inject.{AbstractModule, Inject}
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import java.io.{InputStreamReader, UnsupportedEncodingException, IOException}
import java.net._
import java.nio.{BufferOverflowException, CharBuffer}
import java.nio.charset.Charset
import nu.validator.htmlparser.common.{Heuristics, XmlViolationPolicy}
import nu.validator.htmlparser.sax.HtmlParser
import org.apache.http.client.ClientProtocolException
import org.apache.http.client.methods.{HttpGet, HttpHead}
import org.apache.http.HttpResponse
import org.apache.http.util.EntityUtils
import org.xml.sax.{InputSource, SAXException}
import util.control.Breaks._
import Util._
import com.google.inject.multibindings.Multibinder
import javax.inject.Singleton
import org.apache.http.conn.HttpHostConnectException

object UrlInfoCommon extends Logger {

	// TODO
	final val MAX_URL_INFO_LENGTH = 460
	// TODO find in prase loop
	final val IGNORE_URLS_REGEX = Pattern.compile("(^|\\s)!nl($|\\s)")
	final val URL_PROTO_REGEX = Pattern.compile("\\bhttps?://")
	final val CHARSET_REGEX = Pattern.compile("charset=(\\S+)")
	final val DEFAULT_HTTP_CHARSET = Charset.forName("latin1")
	final val CTTYPE_HEADER = "Content-Type"
	final val TITLE_PREFIX = "title: "
	final val LONG_MSG_SUFFIX = "..."

	val schemeRegistry = {
		val sslCtx = SSLContext.getInstance("TLS")

		sslCtx.init(null, Array[TrustManager](
			new X509TrustManager() {
				def checkClientTrusted(p1: Array[X509Certificate], p2: String) {}

				def checkServerTrusted(p1: Array[X509Certificate], p2: String) {}

				def getAcceptedIssuers = null
			}
		), null)

		val sslSockFactory = new SSLSocketFactory(sslCtx, SSLSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER)

		val schemes = new SchemeRegistry
		schemes.register(new Scheme("http", 80, PlainSocketFactory.getSocketFactory))
		schemes.register(new Scheme("https", 443, sslSockFactory))

		schemes
	}

	def findUrls(line: String) = {
		def iter(results: List[URI], matcher: Matcher, start: Int): List[URI] = {
			def addUri(results: List[URI], s: String): List[URI] =
				parseUri(s) match {
					case Some(uri) => uri :: results
					case None => results
				}

			if (start < line.length && matcher.find(start)) {
				val (mStart, mEnd) = (matcher.start, matcher.end)

				if (mStart == 0 || line(mStart - 1) != '!') {
					val urlEnd =
						if (mStart != 0 && line(mStart - 1) == '<') {
							val pos = line.indexOf('>', mEnd)
							if (pos < 0)
								line.length
							else
								pos
						} else {
							val wsPos = {
								val pos = line.indexWhere(c => Character.isWhitespace(c), mEnd)
								if (pos < 0)
									line.length
								else
									pos
							}
							// don't match last dot in "I often visit http://www.zombo.com."
							if (line(wsPos - 1) == '.')
								wsPos - 1
							else
								wsPos
						}

					val newResults = addUri(results, line.substring(mStart, urlEnd))
					iter(newResults, matcher, urlEnd + 1)
				} else {
					iter(results, matcher, mEnd)
				}
			} else {
				results
			}
		}

		iter(Nil, URL_PROTO_REGEX.matcher(line), 0).reverse
	}

	/*
	 * This sucks because HttpClient accepts only java.net.URI, URL#toURI()
	 * is broken (doesn't encode path & query) and URI constructor encodes
	 * path & query so we need to decode it first so already encoded parts
	 * will not be encoded twice.
	 */
	def parseUri(s: String): Option[URI] = {
		val url = try {
			new URL(s)
		} catch {
			case e: MalformedURLException => {
				logWarn("Couldn't parse URL " + s, e)
				return None
			}
		}

		def decodePart(s: String): String = {
			if (s == null)
				null
			else
				URLDecoder.decode(s, "utf-8")
		}

		val (path, query) = try {
			(decodePart(url.getPath), decodePart(url.getQuery))
		} catch {
			case e: IllegalArgumentException => {
				logWarn("Couldn't decode URL " + s, e)
				return None
			}
		}

		val uri = try {
			new URI(
				url.getProtocol,
				url.getAuthority,
				path,
				query,
				null)
		} catch {
			case e: URISyntaxException => {
				logWarn("Couldn't create URI " + s, e)
				return None
			}
		}

		Some(uri)
	}

	def origNetEx(ex: Throwable) =
		ex match {
			case _: HttpHostConnectException => ex.getCause
			case _ => ex
		}

	@inline
	def logUriError(uri: URI, err: String, ex: Throwable = null) {
		logWarn(s"URI: $uri: $err", ex)
	}
}

object UrlInfo {

	def apply(dlLimit: Long = 30000, connTimeout: Long = 15000, soTimeout: Long = 15000) =
		new UrlInfo(UrlInfoConfig(dlLimit, connTimeout, soTimeout))
}

final class UrlInfo(config: UrlInfoConfig) extends AbstractModule {

	def configure() {
		bind(classOf[UrlInfoConfig]).toInstance(config)
		Multibinder.newSetBinder(binder, classOf[Service]).addBinding.to(classOf[UrlInfoHandlers])
		Multibinder.newSetBinder(binder, classOf[ModuleHandlers]).addBinding.to(classOf[UrlInfoHandlers])
	}
}

final case class UrlInfoConfig(dlLimit: Long, connTimeout: Long, soTimeout: Long)

@Singleton
final class UrlInfoHandlers @Inject()(val config: UrlInfoConfig) extends ModuleHandlers with Service {

	import UrlInfoCommon._

	val httpClient = {
		val connMgr = new PoolingClientConnectionManager(schemeRegistry)

		val params = new BasicHttpParams
		HttpConnectionParams.setConnectionTimeout(params, config.connTimeout.toInt)
		HttpConnectionParams.setSoTimeout(params, config.soTimeout.toInt)

		new DefaultHttpClient(connMgr, params)
	}

	private val connMonitor = new AsyncLoop(10 * 60 * 1000, httpClient.getConnectionManager.closeExpiredConnections)

	def stop {
		connMonitor.stop
		httpClient.getConnectionManager.shutdown
	}

	override val message = Some(new IrcEventHandler[IrcMessageEvent] {

		def handle(event: IrcMessageEvent) = {
			for (info <- new UrlInfoGen(event.message, UrlInfoHandlers.this)) {
				event.context.bot.sendMessage(event.channel, info)
			}

			true
		}
	})
}

final class UrlInfoMessageBuffer(val real: CharBuffer) {

	val overflowEx = new Exception

	def append(c: Char) {
		try {
			real.append(c)
		} catch {
			case _: BufferOverflowException => throw overflowEx
		}
	}

	def append(s: CharSequence) {
		try {
			real.append(s)
		} catch {
			case _: BufferOverflowException => throw overflowEx
		}
	}
}

final class UrlInfoGen(message: String, main: UrlInfoHandlers) extends Traversable[String] with Logger {

	import UrlInfoCommon._

	def foreach[U](f: String => U) {
		if (IGNORE_URLS_REGEX.matcher(message).find)
			return

		for (uri <- findUrls(message)) {
			var msg: Option[CharSequence] = None

			breakable {
				val (response, getResp) = try {
					val head = main.httpClient.execute(new HttpHead(uri))
					val getStat = head.getStatusLine.getStatusCode

					if (getStat == 404 || getStat == 405) {
						val get = main.httpClient.execute(new HttpGet(uri))
						(get, Some(get))
					} else {
						(head, None)
					}
				} catch {
					case e@(_: IOException | _: ClientProtocolException) => {
						msg = Some("request failed: " + origNetEx(e))
						logUriError(uri, "Request failed", e)
						break
					}
				}

				val statCode = response.getStatusLine.getStatusCode

				val buf = new UrlInfoMessageBuffer(CharBuffer.allocate(MAX_URL_INFO_LENGTH))
				buf.real.limit(buf.real.capacity - LONG_MSG_SUFFIX.length)

				if (statCode != 200) {
					buf.append(statCode.toString)
					buf.append(' ')
					buf.append(response.getStatusLine.getReasonPhrase)
				}

				try {
					// will HttpClient ever return 3xx?
					if (statCode == 300 || statCode == 301 || statCode == 302 || statCode == 303 || statCode == 307) {
						val locHeader = response.getFirstHeader("Location")

						if (locHeader != null) {
							buf.append("; location: ")
							buf.append(locHeader.getValue)
						}
					} else if (statCode == 200 || statCode == 203) {
						if (statCode != 200)
							buf.append("; ")

						val ctHeader = response.getFirstHeader(CTTYPE_HEADER)

						if (ctHeader == null) {
							buf.append("no Content-Type")
						} else {
							val ct = ctHeader.getValue

							def appendGeneralUriInfo {
								buf.append("content: ")
								buf.append(ct)

								val cLenHeader = response.getFirstHeader("Content-Length")
								if (cLenHeader != null) {
									buf.append(", size: ")

									try {
										buf.append(prefixUnit(cLenHeader.getValue.toLong, "B"))
									} catch {
										case _: NumberFormatException => buf.append("not an integer")
									}
								}
							}

							if (ct.startsWith("text/html") || ct.startsWith("application/xhtml+xml")) {
								try {
									val oldPos = buf.real.position
									appendTitle(uri, ct, buf, getResp)
									if (buf.real.position == oldPos)
										appendGeneralUriInfo
								} catch {
									case e@(_: IOException | _: ClientProtocolException | _: SAXException) => {
										logUriError(uri, "Failed to get/parse page", e)
										appendGeneralUriInfo
									}
								}
							} else {
								appendGeneralUriInfo
							}
						}
					}

					buf.real.flip
					msg = Some(buf.real)
				} catch {
					case buf.overflowEx => {
						buf.real.limit(buf.real.capacity)
						buf.real.append(LONG_MSG_SUFFIX)

						buf.real.flip
						msg = Some(buf.real)
					}
				} finally {
					if (getResp.isDefined) {
						try {
							EntityUtils.consume(getResp.get.getEntity) // just closes the stream
						} catch {
							case e: IOException => logUriError(uri, "Failed to close input stream", e)
						}
					}
				}
			}

			if (msg.isDefined)
				f(msg.get.toString)
		}
	}

	private def appendTitle(uri: URI, ct: String, buf: UrlInfoMessageBuffer, getResp: Option[HttpResponse]) {
		val input = getResp.getOrElse {
			main.httpClient.execute(new HttpGet(uri))
		}.getEntity.getContent

		try {
			val httpCharset: Option[Charset] = {
				val matcher = CHARSET_REGEX.matcher(ct)
				if (matcher.find) {
					val name = matcher.group(1)
					try {
						Some(Charset.forName(name))
					} catch {
						case e: UnsupportedEncodingException => {
							logUriError(uri, s"Unknown encoding: $name", e)
							return
						}
					}
				} else {
					None
				}
			}

			val parser = new HtmlParser(XmlViolationPolicy.ALLOW)
			parser.setContentHandler(new UrlInfoTitleExtractor(buf))

			val limInput = new LimitedInputStream(input, main.config.dlLimit)
			val xmlSource =
				if (httpCharset.isDefined) {
					parser.setHeuristics(Heuristics.NONE)
					new InputSource(new InputStreamReader(limInput, httpCharset.get))
				} else {
					parser.setHeuristics(Heuristics.ICU)
					new InputSource(limInput)
				}

			breakable {
				parser.parse(xmlSource)
			}
		} finally {
			input.close
		}
	}
}

final class UrlInfoTitleExtractor(buf: UrlInfoMessageBuffer) extends DefaultHandler {

	private object State extends Enumeration {
		type State = Value
		val INIT, IN_HTML, IN_HEAD, IN_TITLE = Value
	}

	private object TitleState extends Enumeration {
		type TitleState = Value
		val TITLE_INIT, TITLE_TEXT, TITLE_SPACE = Value
	}

	import State._
	import TitleState._

	private var state = INIT
	private var titleState = TITLE_INIT

	override def startElement(uri: String, localName: String, qName: String, attributes: Attributes) {
		if (state == INIT) {
			if (localName.equalsIgnoreCase("html"))
				state = IN_HTML
			else
				break
		} else if (state == IN_HTML) {
			if (localName.equalsIgnoreCase("head"))
				state = IN_HEAD
			else
				break
		} else if (state == IN_HEAD) {
			if (localName.equalsIgnoreCase("title"))
				state = IN_TITLE
		}
	}

	override def endElement(uri: String, localName: String, qName: String) {
		if ((state == IN_TITLE && localName.equalsIgnoreCase("title"))
				|| (state == IN_HEAD && localName.equalsIgnoreCase("head"))
				|| (state != IN_HEAD))
			break
	}

	override def characters(ch: Array[Char], start: Int, length: Int) {
		if (state == IN_TITLE) {
			for (i <- 0 until length) {
				val c = ch(start + i)

				if (Character.isWhitespace(c)) {
					if (titleState == TITLE_TEXT)
						titleState = TITLE_SPACE
				} else {
					if (titleState == TITLE_INIT) {
						buf.append(UrlInfoCommon.TITLE_PREFIX)
						titleState = TITLE_TEXT
					} else if (titleState == TITLE_SPACE) {
						buf.append(' ')
						titleState = TITLE_TEXT
					}

					buf.append(c)
				}
			}
		}
	}
}
