package alisa.modules.urlinfo.default

import java.io._
import org.xml.sax.{SAXException, InputSource}
import java.net.HttpURLConnection
import alisa.util.Misc._
import java.nio.charset.{IllegalCharsetNameException, Charset}
import nu.validator.htmlparser.sax.HtmlParser
import nu.validator.htmlparser.common.{Heuristics, XmlViolationPolicy}
import alisa.util.{MircColors => MC, Logger, LimitedInputStream, MessageBuffer}
import MessageBuffer._
import java.nio.CharBuffer
import alisa.modules.urlinfo.{Config, Common}

object DefaultInfo extends Logger {

	import Common._

	def fill(msgBuf: CharBuffer, config: Config, httpConn: HttpURLConnection) {
		var firstInfo =
			if (httpConn.getResponseCode != 200) {
				addStatus(msgBuf, httpConn)
				false
			} else {
				true
			}

		def addField(name: String, value: String) {
			if (firstInfo)
				firstInfo = false
			else
				msgBuf ++= ", "
			msgBuf ++= name ++= ": " ++= value
		}

		val url = httpConn.getURL
		val ct = httpConn.getHeaderField("Content-Type")

		def addHttpInfo() {
			msgBuf ++= MC(MC.GREY)

			if (ct != null)
				addField("content", ct)

			val cl = httpConn.getHeaderField("Content-Length")
			if (cl != null) {
				logDebug(s"URL $url, Content-Length: $cl")
				try {
					val len = cl.toLong
					addField("size", prefixUnit(len, "B", " "))
				} catch {
					case e: NumberFormatException =>
				}
			}

			val cd = httpConn.getHeaderField("Content-Disposition")
			if (cd != null) {
				val (dtype :: params) = cd.trim.split("\\s*?;\\s*?").toList
				if (dtype.equals("attachment")) {
					def appendFilename(params: List[String]) {
						params match {
							case (p :: xs) =>
								val PREFIX = "filename="
								if (p.startsWith(PREFIX)) {
									addField("filename", p.substring(PREFIX.length))
								} else {
									appendFilename(xs)
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
				val charsetOpt: Option[Charset] = {
					val matcher = CHARSET_REGEX.matcher(ct)
					if (matcher.find) {
						val matched = matcher.group(1)
						val name =
							if (matched.startsWith("\"") && matched.endsWith("\""))
								matched.substring(1, matched.length - 1)
							else
								matched
						logDebug(s"URL $url, HTML charset: $name")
						try {
							Some(Charset.forName(name))
						} catch {
							case e@(_: UnsupportedEncodingException |
							        _: IllegalCharsetNameException) =>
								logDebug(s"URL $url, unsupported/illegal HTML charset", e)
								None
						}
					} else {
						None
					}
				}

				val titleBuf = CharBuffer.allocate(msgBuf.remaining)

				val (charset, mkHandler) = charsetOpt.fold {
					val c = RecodingHandler.DEF_CHARSET
					val h = () => new RecodingHandler(titleBuf): TitleHandler
					(c, h)
				} {
					val h = () => new DumbHandler(titleBuf)
					(_, h)
				}

				val parser = new HtmlParser(XmlViolationPolicy.ALLOW)
				parser.setHeuristics(Heuristics.NONE)

				/*
				  From HtmlParser javadoc:
					By default, this parser doesn't do true streaming but buffers everything
					first. The parser can be made truly streaming by calling
					setStreamabilityViolationPolicy(XmlViolationPolicy.FATAL).
					This has the consequence that errors that require non-streamable recovery
					are treated as fatal.
				*/

				// Initial size of 8192 should be enough most of the time
				val inputBuf = new BufferedInputStream(new LimitedInputStream(
					httpConn.getInputStream, config.dlLimit))
				inputBuf.mark(Int.MaxValue)

				def source = new InputSource(new InputStreamReader(inputBuf, charset))

				def extractTitle(): CharSequence = {
					val handler = mkHandler()
					try {
						parser.setContentHandler(handler)
						parser.parse(source)
					} catch {
						case handler.breakEx =>
					}
					handler.title
				}

				parser.setStreamabilityViolationPolicy(XmlViolationPolicy.FATAL)
				val title =
					try {
						extractTitle()
					} catch {
						case e: SAXException =>
							logDebug(s"Parsing $url", e)
							parser.setStreamabilityViolationPolicy(XmlViolationPolicy.ALLOW)
							inputBuf.reset()
							extractTitle()
					}

				if (title.length == 0)
					addHttpInfo()
				else
					msgBuf ++= title
			} catch {
				case e@(_: IOException | _: SAXException) =>
					logDebug(s"URL $url, IO or HTML parse error", e)
					addHttpInfo()
			}
		} else {
			addHttpInfo()
		}
	}
}
