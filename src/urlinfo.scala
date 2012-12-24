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
import java.lang.Math
import java.io.{InputStreamReader, UnsupportedEncodingException, IOException}
import java.net._
import java.nio.CharBuffer
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

object UrlInfoCommon extends Logger {

	final val MAX_URL_INFO_LENGTH = 512
	final val IGNORE_URLS_REGEX = Pattern.compile("(^|\\s)!nl($|\\s)") // TODO find in prase loop
	final val URL_PROTO_REGEX = Pattern.compile("\\bhttps?\\b")
	final val CHARSET_REGEX = Pattern.compile("charset=(\\S+)")
	final val DEFAULT_HTTP_CHARSET = Charset.forName("latin1")
	final val CTTYPE_HEADER = "Content-Type"

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
		def iter(results: List[URI], matcher: Matcher, start: Int, noEndQuote: Boolean): List[URI] = {
			def addUri(results: List[URI], s: String): List[URI] =
				parseUri(s) match {
					case Some(uri) => uri :: results
					case None => results
				}

			if (matcher.find(start)) {
				val (mStart, mEnd) = (matcher.start, matcher.end)

				if (mStart == 0 || line.charAt(mStart - 1) != '!') {
					if (mStart != 0 && line.charAt(mStart - 1) == '<') {
						if (noEndQuote) {
							iter(results, matcher, mEnd, noEndQuote)
						} else {
							val quoteEnd = line.indexOf('>', mEnd)
							if (quoteEnd < 0) {
								iter(results, matcher, mEnd, false)
							} else {
								val newResults = addUri(results, line.substring(mStart, quoteEnd))
								iter(newResults, matcher, quoteEnd + 1, noEndQuote)
							}
						}
					} else {
						val urlEnd = {
							val pos = line.indexWhere(c => Character.isWhitespace(c) || c == '<' || c == '>', mEnd)
							if (pos < 0)
								line.length - 1
							else
								pos
						}
						val newResults = addUri(results, line.substring(mStart, urlEnd))
						iter(newResults, matcher, urlEnd + 1, noEndQuote)
					}
				} else {
					iter(results, matcher, mEnd, noEndQuote)
				}
			} else {
				results
			}
		}

		iter(Nil, URL_PROTO_REGEX.matcher(line), 0, false)
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
}

object UrlInfo {

	def apply(dlLimit: Long = 30000, connTimeout: Long = 15000, soTimeout: Long = 15000) =
		new UrlInfo(UrlInfoConfig(dlLimit, connTimeout, soTimeout))
}

final class UrlInfo(config: UrlInfoConfig) extends AbstractModule {

	def configure() {
		bind(classOf[UrlInfoContext]).toInstance(createContext(config))
		Multibinder.newSetBinder(binder, classOf[Service]).addBinding.to(classOf[UrlInfoHandlers])
		Multibinder.newSetBinder(binder, classOf[ModuleHandlers]).addBinding.to(classOf[UrlInfoHandlers])
	}

	def createContext(config: UrlInfoConfig) = {
		val noTitleMsg = "HTML with no/empty title in head or in first " + Util.prefixUnit(config.dlLimit, "B")
		new UrlInfoContext(config, noTitleMsg)
	}
}

final case class UrlInfoConfig(dlLimit: Long, connTimeout: Long, soTimeout: Long)

final case class UrlInfoContext(config: UrlInfoConfig, noTitleMsg: String)

@Singleton
final class UrlInfoHandlers @Inject()(val context: UrlInfoContext) extends ModuleHandlers with Service {

	val httpClient = {
		val connMgr = new PoolingClientConnectionManager(UrlInfoCommon.schemeRegistry)

		val params = new BasicHttpParams
		HttpConnectionParams.setConnectionTimeout(params, context.config.connTimeout.toInt)
		HttpConnectionParams.setSoTimeout(params, context.config.soTimeout.toInt)

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
						msg = Some("request failed: " + e)
						logWarn("Request failed for URI " + uri, e)
						break
						throw new IllegalStateException // XXX Scala 2.9
					}
				}

				val statCode = response.getStatusLine.getStatusCode
				val buf = new StringBuilder(MAX_URL_INFO_LENGTH)
				msg = Some(buf)

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
							if (ct.startsWith("text/html") || ct.startsWith("application/xhtml+xml")) {
								try {
									buf.append(getTitle(uri, ct, MAX_URL_INFO_LENGTH - buf.size, getResp))
								} catch {
									case e@(_: IOException | _: ClientProtocolException | _: SAXException) => {
										msg = Some("failed to get/parse page: " + e)
										e.printStackTrace
										break
									}
								}
							} else {
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
						}
					}
				} finally {
					if (getResp.isDefined)
						EntityUtils.consume(getResp.get.getEntity) // just closes the stream
				}
			}

			if (msg.isDefined)
				f(msg.get.toString)
		}

	}

	private def getTitle(uri: URI, ct: String, maxTitleLen: Int, getResp: Option[HttpResponse]): CharSequence = {
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
						case _: UnsupportedEncodingException => return "unknown page encoding: " + name
					}
				} else {
					None
				}
			}

			val titlePrefix = "title: "
			val buf = CharBuffer.allocate(maxTitleLen)
			buf.append(titlePrefix)

			val parser = new HtmlParser(XmlViolationPolicy.ALLOW)
			parser.setContentHandler(new UrlInfoTitleExtractor(buf))

			val limInput = new LimitedInputStream(input, main.context.config.dlLimit)
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

			if (buf.position == titlePrefix.length)
				return main.context.noTitleMsg

			buf.flip
			buf
		} finally {
			input.close
		}
	}
}

final class UrlInfoTitleExtractor(buf: CharBuffer) extends DefaultHandler {

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
			val remaining = Math.min(buf.remaining - 3, length)
			appendTitleText(ch, start, remaining)

			if (remaining < length) {
				buf.append("...")
				break
			}
		}
	}

	private def appendTitleText(ch: Array[Char], start: Int, length: Int) {
		for (i <- 0 until length) {
			val c = ch(start + i)

			if (Character.isWhitespace(c)) {
				if (titleState == TITLE_TEXT)
					titleState = TITLE_SPACE
			} else {
				if (titleState != TITLE_TEXT) {
					if (titleState == TITLE_SPACE)
						buf.put(' ')

					titleState = TITLE_TEXT
				}

				buf.put(c)
			}
		}
	}
}
