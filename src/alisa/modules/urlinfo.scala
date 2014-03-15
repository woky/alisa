package alisa.modules

import java.util.regex.{Matcher, Pattern}
import javax.net.ssl._
import java.security.cert.X509Certificate
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import java.io.{InputStreamReader, UnsupportedEncodingException, IOException}
import java.net._
import java.nio.{BufferOverflowException, CharBuffer}
import java.nio.charset.{IllegalCharsetNameException, Charset}
import nu.validator.htmlparser.common.{Heuristics, XmlViolationPolicy}
import nu.validator.htmlparser.sax.HtmlParser
import org.xml.sax.{InputSource, SAXException}
import alisa._
import alisa.util.Misc
import Misc._
import annotation.tailrec
import scala.Some
import alisa.util.{Logger, LimitedInputStream}
import scala.collection.JavaConversions._

final class UrlInfoProvider extends ModuleProvider {

	def DEF_DL_LIMIT = 100000

	def DEF_CONN_TIMEOUT = 15000

	def DEF_SO_TIMEOUT = 15000

	val name = "urlinfo"

	def create(params: Map[String, AnyRef]) = {
		val dlLimit = params
				.get("dlLimit")
				.map(_.asInstanceOf[Int])
				.getOrElse(DEF_DL_LIMIT)
		val connTimeout = params
				.get("connTimeout")
				.map(_.asInstanceOf[Int])
				.getOrElse(DEF_CONN_TIMEOUT)
		val soTimeout = params
				.get("soTimeout")
				.map(_.asInstanceOf[Int])
				.getOrElse(DEF_SO_TIMEOUT)
    	val domainBlacklist = params
				.get("host_blacklist")
				.map(_.asInstanceOf[java.util.List[String]].toList)
				.getOrElse(Nil)
				.map(Pattern.compile) // TODO user-friendly PatternSyntaxException
		val config = new UrlInfoConfig(dlLimit, connTimeout, soTimeout, domainBlacklist)
		new UrlInfoModule(config)
	}
}

final case class UrlInfoConfig(dlLimit: Long, connTimeout: Int, soTimeout: Int,
							   hostBlacklist: List[Pattern]) {}

object UrlInfoCommon extends Logger {

	// TODO
	final val MAX_URL_INFO_LENGTH = 460
	final val IGNORE_URLS_TAG = "nl"
	final val URL_PROTO_REGEX = Pattern.compile(s"\\b(?:https?://|$IGNORE_URLS_TAG\\b)")
	final val CHARSET_REGEX = Pattern.compile("charset\\s*=\\s*(\\S+)")
	final val DEFAULT_HTTP_CHARSET = Charset.forName("latin1")
	final val TITLE_PREFIX = "title: "
	final val LONG_MSG_SUFFIX = "..."
	final val MAX_REDIRS = 10

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

				if (line.startsWith(IGNORE_URLS_TAG, mStart)) {
					Nil
				} else if (mStart == 0 || line(mStart - 1) != '!') {
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

	def appendUrlInfo(startUrl: URL, buf: UrlInfoMessageBuffer, config: UrlInfoConfig) {
		@tailrec
		def iter(nextUrl: URL, visited: Set[URI], redirCount: Int) {
			if (config.hostBlacklist.exists(_.matcher(nextUrl.getHost).matches))
				return

			val httpConn = nextUrl.openConnection.asInstanceOf[HttpURLConnection]

			httpConn.setConnectTimeout(config.connTimeout)
			httpConn.setReadTimeout(config.soTimeout)
			// our redirect handling is slower (creating HttpUrlConnection instances)
			//httpConn.setInstanceFollowRedirects(false)

			httpConn.setRequestProperty("Accept", "text/html, text/plain, text/css, text/sgml, */*;q=0.01")
			httpConn.setRequestProperty("User-Agent", "Lynx/2.8.5rel.1 libwww-FM/2.14 SSL-MM/1.4.1 OpenSSL/0.9.7d")
			//httpConn.setRequestProperty("Accept-Encoding", "gzip, bzip2")
			httpConn.setRequestProperty("Accept-Language", "en")

			httpConn match {
				case httpsConn: HttpsURLConnection => {
					httpsConn.setSSLSocketFactory(SSL_SOCK_FACTORY)
					httpsConn.setHostnameVerifier(HOSTNAME_VERIFIER)
				}
				case _ =>
			}

			httpConn.connect

			val statCode = httpConn.getResponseCode
			val statMsg = httpConn.getResponseMessage

			def appendStatusLine {
				buf.append(statCode.toString)
				buf.append(' ')
				buf.append(statMsg)
			}

			if (statCode == 200 || statCode == 203) {
				try {
					var firstInfo =
						if (statCode != 200) {
							appendStatusLine
							false
						} else {
							true
						}

					val ct = httpConn.getHeaderField("Content-Type")

					def appendGeneralUriInfo {
						def appendHeader(name: String, value: String) {
							if (firstInfo)
								firstInfo = false
							else
								buf.append(", ")

							buf.append(name)
							buf.append(": ")
							buf.append(value)
						}

						if (ct != null)
							appendHeader("content", ct)

						val cl = httpConn.getHeaderField("Content-Length")
						if (cl != null) {
							try {
								val len = cl.toLong
								appendHeader("size", prefixUnit(len, "B"))
							} catch {
								case e: NumberFormatException => logUrlError(nextUrl, s"Invalid length: $cl", e)
							}
						}

						val cd = httpConn.getHeaderField("Content-Disposition")
						if (cd != null) {
							val (dtype :: params) = cd.trim.split("\\s*?;\\s*?").toList
							if (dtype.equals("attachment")) {
								def appendFilename(params: List[String]) {
									params match {
										case (p :: xs) => {
											val PREFIX = "filename="
											if (p.startsWith(PREFIX)) {
												appendHeader("filename", p.substring(PREFIX.length))
											} else {
												appendFilename(xs)
											}
										}
										case _ =>
									}
								}
								appendFilename(params)
							}
						}
					}

					if (ct != null && (ct.startsWith("text/html") || ct.startsWith("application/xhtml+xml"))) {
						try {
							val oldPos = buf.real.position

							val httpCharset: Option[Charset] = {
								val matcher = CHARSET_REGEX.matcher(ct)
								if (matcher.find) {
									val matched = matcher.group(1)
									val name =
										if (matched.startsWith("\"") && matched.endsWith("\""))
											matched.substring(1, matched.length - 1)
										else
											matched
									try {
										Some(Charset.forName(name))
									} catch {
										case e@(_: UnsupportedEncodingException |
												_: IllegalCharsetNameException) => {
											logUrlError(nextUrl, s"Unknown encoding: $name", e)
											None
										}
									}
								} else {
									None
								}
							}

							val extractor = new UrlInfoTitleExtractor(buf)
							val parser = new HtmlParser(XmlViolationPolicy.ALLOW)
							parser.setContentHandler(extractor)
							//parser.setStreamabilityViolationPolicy(XmlViolationPolicy.FATAL)

							val limInput = new LimitedInputStream(httpConn.getInputStream, config.dlLimit)
							val xmlSource =
								if (httpCharset.isDefined) {
									parser.setHeuristics(Heuristics.NONE)
									new InputSource(new InputStreamReader(limInput, httpCharset.get))
								} else {
									parser.setHeuristics(Heuristics.ICU)
									new InputSource(limInput)
								}

							try {
								parser.parse(xmlSource)
							} catch {
								case extractor.breakEx =>
								case buf.overflowEx =>
							}

							if (buf.real.position == oldPos)
								appendGeneralUriInfo
						} catch {
							case e@(_: IOException | _: SAXException) => {
								logUrlError(nextUrl, "Failed to get/parse page", e)
								appendGeneralUriInfo
							}
						}
					} else {
						appendGeneralUriInfo
					}
				} finally {
					httpConn.disconnect
				}
			} else {
				httpConn.disconnect

				// only for HTTP -> HTTPS or vice versa
				if (statCode == 300 || statCode == 301 || statCode == 302 || statCode == 303 || statCode == 307) {
					if (redirCount < MAX_REDIRS) {
						val location = httpConn.getHeaderField("Location")
						if (location != null) {
							logDebug(s"redirecting to $location")
							val newOptUrl = parseUri(location)
							newOptUrl match {
								case Some(newUri) =>
									if (!visited.contains(newUri)) {
										iter(newUri.toURL, visited + newUri, redirCount + 1)
									} else {
										buf.append("cyclic redirects")
									}
								case None => buf.append("invalid redirect")
							}
						} else {
							buf.append("invalid redirect")
						}
					} else {
						buf.append(s"too many redirects ($MAX_REDIRS)")
					}
				} else {
					appendStatusLine
				}
			}
		}

		CookieHandler.setDefault(new CookieManager())
		iter(startUrl, Set(), 0)
	}
}

final class UrlInfoModule(val config: UrlInfoConfig) extends Module with IrcEventHandler {

	override def handler = Some(this)

	override def handles = Set(classOf[IrcMessageEvent])

	override def handle = {
		case e: IrcMessageEvent => {
			import UrlInfoCommon._

			val buf = CharBuffer.allocate(MAX_URL_INFO_LENGTH)
			val exBuf = new UrlInfoMessageBuffer(buf)

			def allowedUrl(url: URL): Boolean = {
				try {
					InetAddress.getAllByName(url.getHost)
							.forall(a => !a.isLoopbackAddress && !a.isSiteLocalAddress)
				} catch {
					case _: UnknownHostException => true
				}
			}

			for (url <- findUrls(e.message.decoded).filter(allowedUrl)) {
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
							"request failed: " + (e match {
								case e: UnknownHostException => "invalid hostname " + e.getMessage
								case e@(_: SocketTimeoutException | _: SocketException) => e.getMessage
								case _ => {
									logUrlError(url, "Request failed", e)
									e.getMessage
								}
							})
						}
					}

				if (msg.length > 0)
					e.network.bot.sendMessage(e.channel, msg.toString)
			}

			true
		}
	}
}

final class UrlInfoMessageBuffer(val real: CharBuffer) {

	val overflowEx = new RuntimeException

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

	val breakEx = new IOException

	private def break {
		throw breakEx
	}

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
