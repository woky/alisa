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
import resource._
import java.util.zip.{DeflaterInputStream, GZIPInputStream}

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
		val ct = httpConn.getContentType

		def addHttpInfo() {
			msgBuf ++= MC(MC.GREY)

			if (ct != null)
				addField("content", ct)

			httpConn.getContentLengthLong match {
				case -1 =>
				case len => addField("size", prefixUnit(len, "B", " "))
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

		def createInputBuffer() =
			managed(httpConn.getInputStream) map { raw =>
				val decoded = httpConn.getContentEncoding match {
					case null | "identity" => raw
					case "gzip" => new GZIPInputStream(raw)
					case "deflate" => new DeflaterInputStream(raw)
					case e => throw new IOException(s"Unsupported HTTP content encoding [$e]")
				}
				// initial buffer size 8192 should be enough most of the time
				new BufferedInputStream(new LimitedInputStream(decoded, config.dlLimit))
			}

		if (ct != null && (ct.startsWith("text/html") || ct.startsWith("application/xhtml+xml"))) {
			try {
				createInputBuffer() map { inputBuf => // transform to title
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

					/*
					  Try to parse in streaming mode and fall back to tree mode.
					  From HtmlParser javadoc:
						By default, this parser doesn't do true streaming but buffers
						everything first. The parser can be made truly streaming by calling
						setStreamabilityViolationPolicy(XmlViolationPolicy.FATAL). This has the
						consequence that errors that require non-streamable recovery are
						treated as fatal.
					*/

					// directly passed to SAX handler, but may be used twice
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

					// we don't want the parser to close input because we may reset() it
					def source = new InputSource(new FilterReader(new InputStreamReader(
						inputBuf, charset)) {
						override def close() {}
					})

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

					inputBuf.mark(Int.MaxValue)
					parser.setStreamabilityViolationPolicy(XmlViolationPolicy.FATAL)
					try {
						extractTitle()
					} catch {
						case e: SAXException =>
							logDebug(s"Parsing $url", e)
							parser.setStreamabilityViolationPolicy(XmlViolationPolicy.ALLOW)
							inputBuf.reset()
							extractTitle()
					}
				} foreach {
					title =>
						if (title.length == 0)
							addHttpInfo()
						else
							msgBuf ++= title
				}
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
