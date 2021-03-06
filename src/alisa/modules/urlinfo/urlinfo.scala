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
import alisa.util.{Logger, MessageBuffer}
import MessageBuffer._
import scala.collection.JavaConverters._
import scala.util.{Success, Failure, Try}
import alisa.modules.urlinfo.default.DefaultInfo

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
				.map(_.asInstanceOf[java.util.List[String]].asScala.toList)
				.getOrElse(Nil)
				.map(Pattern.compile) // TODO user-friendly PatternSyntaxException
		val optYtApiKey = params
				.get("youtube_api_key")
				.map(_.asInstanceOf[String])
		val optScClientId = params
				.get("soundcloud_client_id")
				.map(_.asInstanceOf[String])
		val config = new Config(dlLimit, connTimeout, soTimeout, hostBlacklist, optYtApiKey,
			optScClientId)
		new UrlInfoModule(config)
	}
}

final case class Config(dlLimit: Long, connTimeout: Int, soTimeout: Int,
							   hostBlacklist: List[Pattern], optYtApiKey: Option[String],
							   optScClientId: Option[String]) {}

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
		YouTube,
		SoundCloud,
		GitHub
	)

	private[this] val sslCtx = {
		val sslCtx = SSLContext.getInstance("TLS")
		sslCtx.init(null, Array[TrustManager](
			new X509TrustManager() {
				def checkClientTrusted(p1: Array[X509Certificate], p2: String) {}

				def checkServerTrusted(p1: Array[X509Certificate], p2: String) {}

				def getAcceptedIssuers = Array()
			}
		), null)
		sslCtx
	}

	val SSL_SOCK_FACTORY = sslCtx.getSocketFactory

	val HOSTNAME_VERIFIER = new HostnameVerifier {
		def verify(hostname: String, session: SSLSession) = true
	}

	def findUrls(line: String) = {
		@tailrec
		def iter(results: List[URL], matcher: Matcher, start: Int): List[URL] = {
			if (start < line.length && matcher.find(start)) {
				val (mStart, mEnd) = (matcher.start, matcher.end)
				if (line.startsWith(IGNORE_URLS_TAG, mStart)) {
					Nil
				} else if (mStart == 0 || line(mStart - 1) != '!') {
					val urlEnd = {
						val endOrDot = {
							val wsOrEnd = line.indexWhere(Character.isWhitespace, mEnd)
							if (wsOrEnd < 0)
								line.length
							else
								wsOrEnd
						}
						// don't match last dot in "I often visit http://www.zombo.com."
						if (line(endOrDot - 1) == '.')
							endOrDot - 1
						else
							endOrDot
					}
					val strUrl = line.substring(mStart, urlEnd)
					Try(new URL(strUrl)) match {
						case Success(url) => iter(url :: results, matcher, urlEnd + 1)
						case Failure(ex) =>
							logDebug("Couldn't parse URL: [" + strUrl + "]", ex)
							iter(results, matcher, urlEnd + 1)
					}
				} else {
					iter(results, matcher, mEnd)
				}
			} else {
				results
			}
		}
		iter(Nil, URL_PROTO_REGEX.matcher(line), 0).reverse
	}

	def addStatus(buf: MessageBuffer, httpConn: HttpURLConnection): buf.type = {
		buf ++= "Status: " ++= httpConn.getResponseCode
		httpConn.getResponseMessage match {
			case null =>
			case msg => buf += ' ' ++= msg
		}
		buf
	}

}

final class UrlInfoModule(val config: Config) extends Module with IrcEventHandler
															 with Logger {
	import Common._

	System.setProperty("sun.net.http.errorstream.enableBuffering", "true")

	override def handler = Some(this)

	override def handles = Set(classOf[IrcMessageEvent])

	override def handle = {
		case evt: IrcMessageEvent =>
			import Common._

			val buf = CharBuffer.allocate(MAX_MSG_LEN)

			for (url <- findUrls(evt.message.decoded)) {
				val ELLIPSIS = "…"
				buf.position(0)
				buf.limit(buf.capacity - 2 /* ELLIPSIS byte length */)

				val msg: CharSequence =
					try {
						fill(buf, url)
						buf.flip
						buf
					} catch {
						case e: BufferOverflowException =>
							buf.limit(buf.capacity)
							buf.append(ELLIPSIS)
							buf.flip
							buf
						case e: IOException => "ERROR: " + e.toString
					}

				if (msg.length > 0)
					evt.bot.sendMessage(evt.channel, msg.toString)
			}

			true
	}

	private def isLocalUrl(url: URL): Boolean =
		InetAddress.getAllByName(url.getHost)
			.exists(addr => addr.isLoopbackAddress || addr.isSiteLocalAddress)

	private def isBlacklisted(url: URL) =
		config.hostBlacklist.exists(_.matcher(url.getHost).matches)

	private def fill(buf: CharBuffer, startUrl: URL) {
		@tailrec
		def iterUrls(nextUrl: URL, visited: Set[URL], redirCount: Int) {
			if (isLocalUrl(nextUrl) || isBlacklisted(nextUrl)) {
				buf ++= f"ERROR: Not going to visit $nextUrl"
				return
			}
			if (HANDLERS.exists(handler => {
				buf.position(0)
				handler.fill(buf, config, nextUrl)
			}))
				return

			val httpConn = nextUrl.openConnection.asInstanceOf[HttpURLConnection]

			httpConn.setConnectTimeout(config.connTimeout)
			httpConn.setReadTimeout(config.soTimeout)
			httpConn.setInstanceFollowRedirects(false)

			httpConn.setRequestProperty("Accept", "text/html, text/plain, text/css, text/sgml, */*;q=0.01")
			httpConn.setRequestProperty("User-Agent", "Lynx/2.8.5rel.1 libwww-FM/2.14 SSL-MM/1.4.1 OpenSSL/0.9.7d")
			httpConn.setRequestProperty("Accept-Encoding", "gzip, deflate")
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
							Try(new URL(location)) match {
								case Success(newUrl) if !visited.contains(newUrl) =>
									iterUrls(newUrl, visited + newUrl, redirCount + 1)
								case Success(newUrl) =>
									buf ++= "ERROR: Cyclic redirect, already visited URL: [" ++=
											newUrl += ']'
								case Failure(_: MalformedURLException) =>
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

trait UrlHandler {

	def fill(buf: CharBuffer, config: Config, url: URL): Boolean
}
