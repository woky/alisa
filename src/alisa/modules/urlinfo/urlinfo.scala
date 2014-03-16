package alisa.modules.urlinfo

import java.util.regex.{Matcher, Pattern}
import javax.net.ssl._
import java.security.cert.X509Certificate
import java.io.IOException
import java.net._
import java.nio.{BufferOverflowException, CharBuffer}
import java.nio.charset.Charset
import alisa._
import annotation.tailrec
import alisa.util.Logger
import scala.collection.JavaConversions._
import scala.util.control.ControlThrowable

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
		val hostBlacklist = params
				.get("host_blacklist")
				.map(_.asInstanceOf[java.util.List[String]].toList)
				.getOrElse(Nil)
				.map(Pattern.compile) // TODO user-friendly PatternSyntaxException
		val optYtApiKey = params
				.get("youtube_api_key")
				.map(_.asInstanceOf[String])
		val config = new Config(dlLimit, connTimeout, soTimeout, hostBlacklist, optYtApiKey)
		new UrlInfoModule(config)
	}
}

final case class Config(dlLimit: Long, connTimeout: Int, soTimeout: Int,
							   hostBlacklist: List[Pattern], optYtApiKey: Option[String]) {}

object Common extends Logger {

	// TODO
	final val MAX_MSG_LEN = 460
	final val IGNORE_URLS_TAG = "nl"
	final val URL_PROTO_REGEX = Pattern.compile(s"\\b(?:https?://|$IGNORE_URLS_TAG\\b)")
	final val CHARSET_REGEX = Pattern.compile("charset\\s*=\\s*(\\S+)")
	final val DEFAULT_HTTP_CHARSET = Charset.forName("latin1")
	final val TITLE_PREFIX = "Title: "
	final val MAX_REDIRS = 10
	
	final val HANDLERS: List[UrlHandler] = List(
		Youtube
	)
	
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

	// TODO No HttpClient involved anymore. Check if it's still relevant and possibly remove it.
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
			case e: MalformedURLException =>
				logWarn("Couldn't parse URL " + s, e)
				return None
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
			case e: IllegalArgumentException =>
				logWarn("Couldn't decode URL " + s, e)
				return None
		}

		val uri = try {
			new URI(
				url.getProtocol,
				url.getAuthority,
				path,
				query,
				null)
		} catch {
			case e: URISyntaxException =>
				logWarn("Couldn't create URI " + s, e)
				return None
		}

		Some(uri)
	}

	def addStatus(buf: MessageBuffer, httpConn: HttpURLConnection): buf.type = {
		buf ++= httpConn.getResponseCode += ' ' ++= httpConn.getResponseMessage
		buf
	}

}

final class UrlInfoModule(val config: Config) extends Module with IrcEventHandler
															 with Logger {
	import Common._

	override def handler = Some(this)

	override def handles = Set(classOf[IrcMessageEvent])

	override def handle = {
		case evt: IrcMessageEvent =>
			import Common._

			val buf = CharBuffer.allocate(MAX_MSG_LEN)
			val exBuf = new MessageBuffer(buf)

			def isAllowedUrl(url: URL): Boolean = {
				!InetAddress.getAllByName(url.getHost).exists(addr => addr.isLoopbackAddress ||
						addr.isSiteLocalAddress)
			}

			for (url <- findUrls(evt.message.decoded).filter(
				url => try {
					isAllowedUrl(url)
				} catch {
					case ex: UnknownHostException =>
						evt.network.bot.sendMessage(evt.channel, "ERROR: Unknown host " +
								s"[${url.getHost}]: [${ex.getMessage}]")
						false
				}
			)) {
				val ELLIPSIS = "…"
				buf.position(0)
				buf.limit(buf.capacity - 2 /* ELLIPSIS byte length */)

				val msg: CharSequence =
					try {
						fill(exBuf, url)
						buf.flip
						buf
					} catch {
						case exBuf.overflowEx =>
							buf.limit(buf.capacity)
							buf.append(ELLIPSIS)
							buf.flip
							buf
						case e: UnknownHostException => s"ERROR: Unkown host [${url.getHost}]: " +
								s"[${e.getMessage}]"
						case e: SocketTimeoutException => s"ERROR: Socket timeout: " +
								s"[${e.getMessage}]"
						case e: SocketException => s"ERROR: Socket error: [${e.getMessage}]"
						case e: IOException => s"ERROR: I/O error: [${e.getMessage}]"
					}

				if (msg.length > 0)
					evt.network.bot.sendMessage(evt.channel, msg.toString)
			}

			true
	}

	private def isBlacklisted(url: URL) =
		config.hostBlacklist.exists(_.matcher(url.getHost).matches)

	private def fill(buf: MessageBuffer, startUrl: URL) {
		@tailrec
		def iterUrls(nextUrl: URL, visited: Set[URI], redirCount: Int) {
			if (isBlacklisted(nextUrl))
				return
			if (HANDLERS.exists(_.fill(buf, config, nextUrl)))
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
				case httpsConn: HttpsURLConnection =>
					httpsConn.setSSLSocketFactory(SSL_SOCK_FACTORY)
					httpsConn.setHostnameVerifier(HOSTNAME_VERIFIER)
				case _ =>
			}

			httpConn.connect
			val statCode = httpConn.getResponseCode

			if (isBlacklisted(httpConn.getURL)) { // url may be different after redirect
				httpConn.disconnect()
				return
			}

			if (statCode == 200 || statCode == 203) {
				try {
					DefaultInfo.fill(buf, config, httpConn)
				} finally {
					httpConn.disconnect
				}
			} else {
				httpConn.disconnect

				// only for HTTP -> HTTPS or vice versa
				if (statCode == 300 || statCode == 301 || statCode == 302 || statCode == 303 ||
						statCode == 307) {
					if (redirCount < MAX_REDIRS) {
						val location = httpConn.getHeaderField("Location")
						if (location != null) {
							logDebug(s"redirecting to $location")
							val newOptUrl = parseUri(location)
							newOptUrl match {
								case Some(newUrl) =>
									if (!visited.contains(newUrl)) {
										iterUrls(newUrl.toURL, visited + newUrl, redirCount + 1)
									} else {
										buf ++= "ERROR: Cyclic redirect, already visited URL: [" ++=
												newUrl += ']'
									}
								case None =>
									buf ++= "ERROR: Couldn't parse Location header URL [" ++=
											location += ']'
							}
						} else {
							buf ++= "ERROR: Empty Location header"
						}
					} else {
						buf ++= s"ERROR: Too many redirects [" ++= MAX_REDIRS += ']'
					}
				} else {
					addStatus(buf, httpConn)
				}
			}
		}

		CookieHandler.setDefault(new CookieManager())
		iterUrls(startUrl, Set(), 0)
	}
}

final class MessageBuffer(val underlying: CharBuffer) {

	val overflowEx = new ControlThrowable {}

	def +=(c: Char): this.type = {
		try {
			underlying.append(c)
			this
		} catch {
			case _: BufferOverflowException => throw overflowEx
		}
	}

	def ++=(s: CharSequence): this.type = {
		try {
			underlying.append(s)
			this
		} catch {
			case _: BufferOverflowException => throw overflowEx
		}
	}

	def ++=(s: Any): this.type = ++=(s.toString)
}

trait UrlHandler {

	def fill(buf: MessageBuffer, config: Config, url: URL): Boolean
}

