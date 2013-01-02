package alisa

import java.util.regex.{Matcher, Pattern}
import javax.net.ssl._
import java.security.cert.X509Certificate
import com.google.inject.{AbstractModule, Inject}
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import java.io.{InputStreamReader, UnsupportedEncodingException, IOException}
import java.net._
import java.nio.{BufferOverflowException, CharBuffer}
import java.nio.charset.Charset
import nu.validator.htmlparser.common.{Heuristics, XmlViolationPolicy}
import nu.validator.htmlparser.sax.HtmlParser
import org.xml.sax.{InputSource, SAXException}
import util.control.Breaks._
import Util._
import com.google.inject.multibindings.Multibinder
import javax.inject.Singleton

object UrlInfoCommon extends Logger {

	// TODO
	final val MAX_URL_INFO_LENGTH = 460
	final val IGNORE_URLS_TAG = "nl"
	final val URL_PROTO_REGEX = Pattern.compile(s"\\b(?:https?://|$IGNORE_URLS_TAG\\b)")
	final val CHARSET_REGEX = Pattern.compile("charset=(\\S+)")
	final val DEFAULT_HTTP_CHARSET = Charset.forName("latin1")
	final val TITLE_PREFIX = "title: "
	final val LONG_MSG_SUFFIX = "..."

	private[this] val sslCtx = {
		val sslCtx = SSLContext.getInstance("TLS")
		sslCtx.init(null, Array[TrustManager](
			new X509TrustManager() {
				def checkClientTrusted(p1: Array[X509Certificate], p2: String) {}

				def checkServerTrusted(p1: Array[X509Certificate], p2: String) {}

				def getAcceptedIssuers = null
			}
		), null)
		sslCtx
	}

	val SSL_SOCK_FACTORY = sslCtx.getSocketFactory

	val HOSTNAME_VERIFIER = new HostnameVerifier {
		def verify(hostname: String, session: SSLSession) = true
	}

	def findUrls(line: String) = {
		def iter(results: List[URL], matcher: Matcher, start: Int): List[URL] = {
			def addUrl(results: List[URL], s: String): List[URL] =
				parseUri(s) match {
					case (Some(uri)) => uri.toURL :: results
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

					val newResults = addUrl(results, line.substring(mStart, urlEnd))
					iter(newResults, matcher, urlEnd + 1)
				} else if (line.startsWith(IGNORE_URLS_TAG, mStart)) {
					Nil
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

	@inline
	def logUrlError(url: URL, err: String, ex: Throwable = null) {
		logWarn(s"URI: $url: $err", ex)
	}

	def appendUrlInfo(url: URL, buf: UrlInfoMessageBuffer, config: UrlInfoConfig) {
		val httpConn = url.openConnection.asInstanceOf[HttpURLConnection]

		httpConn.setConnectTimeout(config.connTimeout)
		httpConn.setReadTimeout(config.soTimeout)

		httpConn match {
			case httpsConn: HttpsURLConnection => {
				httpsConn.setSSLSocketFactory(SSL_SOCK_FACTORY)
				httpsConn.setHostnameVerifier(HOSTNAME_VERIFIER)
			}
			case _ =>
		}

		try {
			httpConn.connect

			val statCode = httpConn.getResponseCode

			if (statCode != 200) {
				buf.append(statCode.toString)
				buf.append(' ')
				buf.append(httpConn.getResponseMessage)
			}

			if (statCode == 300 || statCode == 301 || statCode == 302 || statCode == 303 || statCode == 307) {
				// HttpUrlConnection follows redirects by default but let's keep it
				val location = httpConn.getHeaderField("Location")
				if (location != null) {
					buf.append("; location: ")
					buf.append(location)
				}
			} else if (statCode == 200 || statCode == 203) {
				if (statCode != 200)
					buf.append("; ")

				val ct = httpConn.getHeaderField("Content-Type")
				if (ct == null) {
					buf.append("no Content-Type")
				} else {
					def appendGeneralUriInfo {
						buf.append("content: ")
						buf.append(ct)

						val cLenHeader = httpConn.getHeaderField("Content-Length")
						if (cLenHeader != null) {
							buf.append(", size: ")

							try {
								buf.append(prefixUnit(cLenHeader.toLong, "B"))
							} catch {
								case _: NumberFormatException => buf.append("not an integer")
							}
						}
					}

					if (ct.startsWith("text/html") || ct.startsWith("application/xhtml+xml")) {
						try {
							val oldPos = buf.real.position

							val httpCharset: Option[Charset] = {
								val matcher = CHARSET_REGEX.matcher(ct)
								if (matcher.find) {
									val name = matcher.group(1)
									try {
										Some(Charset.forName(name))
									} catch {
										case e: UnsupportedEncodingException => {
											logUrlError(url, s"Unknown encoding: $name", e)
											return
										}
									}
								} else {
									None
								}
							}

							val parser = new HtmlParser(XmlViolationPolicy.ALLOW)
							parser.setContentHandler(new UrlInfoTitleExtractor(buf))

							val limInput = new LimitedInputStream(httpConn.getInputStream, config.dlLimit)
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

							if (buf.real.position == oldPos)
								appendGeneralUriInfo
						} catch {
							case e@(_: IOException | _: SAXException) => {
								logUrlError(url, "Failed to get/parse page", e)
								appendGeneralUriInfo
							}
						}
					} else {
						appendGeneralUriInfo
					}
				}
			}
		} finally {
			httpConn.disconnect
		}
	}
}

object UrlInfo {

	def apply(dlLimit: Long = 30000, connTimeout: Int = 15000, soTimeout: Int = 15000) =
		new UrlInfo(UrlInfoConfig(dlLimit, connTimeout, soTimeout))
}

final class UrlInfo(config: UrlInfoConfig) extends AbstractModule {

	def configure() {
		bind(classOf[UrlInfoConfig]).toInstance(config)
		Multibinder.newSetBinder(binder, classOf[ModuleHandlers]).addBinding.to(classOf[UrlInfoHandlers])
	}
}

final case class UrlInfoConfig(dlLimit: Long, connTimeout: Int, soTimeout: Int)

@Singleton
final class UrlInfoHandlers @Inject()(val config: UrlInfoConfig) extends ModuleHandlers {

	override val message = Some(new IrcEventHandler[IrcMessageEvent] {

		def handle(event: IrcMessageEvent) = {
			import UrlInfoCommon._

			val buf = CharBuffer.allocate(MAX_URL_INFO_LENGTH)
			val exBuf = new UrlInfoMessageBuffer(buf)

			for (url <- findUrls(event.message)) {
				buf.position(0)
				buf.limit(buf.capacity - LONG_MSG_SUFFIX.length)

				val msg: CharSequence =
					try {
						appendUrlInfo(url, exBuf, config)
						buf.flip
						buf
					} catch {
						case exBuf.overflowEx => {
							buf.limit(buf.capacity)
							buf.append(LONG_MSG_SUFFIX)
							buf.flip
							buf
						}
						case e: IOException => {
							logUrlError(url, "Request failed", e)
							"request failed: " + e
						}
					}

				event.context.bot.sendMessage(event.channel, msg.toString)
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
