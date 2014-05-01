package alisa.modules.urlinfo

import org.xml.sax.helpers.DefaultHandler
import java.io._
import org.xml.sax.{SAXException, InputSource, Attributes}
import java.net.HttpURLConnection
import alisa.util.Misc._
import java.nio.charset.{IllegalCharsetNameException, Charset}
import nu.validator.htmlparser.sax.HtmlParser
import nu.validator.htmlparser.common.{Heuristics, XmlViolationPolicy}
import alisa.util.{MircColors => MC, Logger, LimitedInputStream, MessageBuffer}
import scala.util.control.ControlThrowable

object DefaultInfo extends Logger {
	import Common._

	def fill(buf: MessageBuffer, config: Config, httpConn: HttpURLConnection) {
		var firstInfo =
			if (httpConn.getResponseCode != 200) {
				addStatus(buf, httpConn)
				false
			} else {
				true
			}

		def addField(name: String, value: String) {
			if (firstInfo)
				firstInfo = false
			else
				buf ++= ", "
			buf ++= name ++= ": " ++= value
		}

		val url = httpConn.getURL
		val ct = httpConn.getHeaderField("Content-Type")

		def addHttpInfo() {
			buf ++= MC(MC.GREY)

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
				val oldPos = buf.underlying.position

				val httpCharset: Option[Charset] = {
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

				/*
					from HtmlParser javadoc:
						By default, this parser doesn't do true streaming but buffers everything
						first. The parser can be made truly streaming by calling
						setStreamabilityViolationPolicy(XmlViolationPolicy.FATAL).
						This has the consequence that errors that require non-streamable recovery
						are treated as fatal.
				*/

				// Initial size of 8192 should be enough most of the time
				val bufInput = new BufferedInputStream(new LimitedInputStream(
					httpConn.getInputStream, config.dlLimit))
				bufInput.mark(config.dlLimit.toInt)

				val extractor = new HtmlTitleExtractor(buf)
				val parser = new HtmlParser(XmlViolationPolicy.ALLOW)
				parser.setContentHandler(extractor)
				val xmlSource =
					if (httpCharset.isDefined) {
						parser.setHeuristics(Heuristics.NONE)
						new InputSource(new InputStreamReader(bufInput, httpCharset.get))
					} else {
						parser.setHeuristics(Heuristics.ICU)
						new InputSource(bufInput)
					}

				def extractTitle() {
					try {
						parser.parse(xmlSource)
					} catch {
						case extractor.breakEx =>
						case buf.overflowEx =>
					}
				}

				parser.setStreamabilityViolationPolicy(XmlViolationPolicy.FATAL)
				try {
					extractTitle()
				} catch {
					case e: SAXException =>
						logDebug(s"Parsing $url", e)
						bufInput.reset()
						parser.setStreamabilityViolationPolicy(XmlViolationPolicy.ALLOW)
						extractTitle()
				}

				if (buf.underlying.position == oldPos)
					addHttpInfo()
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

private final class HtmlTitleExtractor(buf: MessageBuffer) extends DefaultHandler {

	val breakEx = new IOException with ControlThrowable

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
						buf ++= Common.TITLE_PREFIX
						titleState = TITLE_TEXT
					} else if (titleState == TITLE_SPACE) {
						buf += ' '
						titleState = TITLE_TEXT
					}

					buf += c
				}
			}
		}
	}
}
