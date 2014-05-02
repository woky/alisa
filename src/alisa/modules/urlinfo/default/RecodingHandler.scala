package alisa.modules.urlinfo.default

import org.xml.sax.Attributes
import alisa.util.Misc._
import java.nio.charset.Charset
import java.nio.CharBuffer
import alisa.modules.urlinfo.Common

private object RecodingHandler {

	final val DEF_CHARSET = Charset.forName("WINDOWS-1252")
}

private final class RecodingHandler(buf: CharBuffer) extends TitleHandler(buf) {

	import RecodingHandler._
	import TitleHandler._
	import StreamState._

	private var charsetOpt: Option[Charset] = None
	private var allAscii = true

	private def findCharset(attrs: Attributes): Option[String] =
		if (attrs.getLength == 1 && (attrs.getLocalName(0) iceq "charset"))
			Some(attrs.getValue(0))
		else if (attrs.getLength == 2) {
			/*
			  Try both
				<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
				<meta content="text/html; charset=utf-8" http-equiv="Content-Type">
			 */
			def tryIndex(idx1: Int) =
				if ((attrs.getLocalName(idx1) iceq "http-equiv")
						&& (attrs.getValue(idx1) iceq "content-type")) {
					val idx2 = (idx1 + 1) % 2
					if (attrs.getLocalName(idx2) iceq "content") {
						val matcher = Common.CHARSET_REGEX.matcher(attrs.getValue(idx2))
						if (matcher.find)
							Some(matcher.group(1))
						else
							None
					} else
						None
				} else
					None

			tryIndex(0) match {
				case None => tryIndex(1)
				case r => r
			}
		} else
			None

	override def startElement(uri: String, name: String, qName: String, attrs: Attributes): Unit =
		streamState match {
			case SS_INIT if "html"  iceq name => streamState = SS_HTML
			case SS_INIT                      => break()
			case SS_HTML if "head"  iceq name => streamState = SS_HEAD
			case SS_HTML                      => break()
			case SS_HEAD if "title" iceq name => streamState = SS_TITLE
			case SS_HEAD if charsetOpt.isEmpty && ("meta" iceq name) =>
				findCharset(attrs) match {
					case Some(csName) =>
						charsetOpt = Some(Charset.forName(csName))
						if (buf.position > 0)
							break()
					case _ =>
				}
			case _ =>
		}

	override protected def checkChar(c: Char): Unit =
		if (allAscii && c >= 0x80)
			allAscii = false

	override def endElement(uri: String, name: String, qName: String): Unit =
		if (streamState == SS_TITLE && ("title" iceq name)) {
			if (charsetOpt.isDefined)
				break()
			else
				streamState = SS_HEAD
		} else if ((streamState == SS_HEAD && ("head" iceq name)) || (streamState != SS_HEAD))
			break()

	// new buffer size should be <= original size since we're recoding from latin1
	override def title = {
		buf.flip
		if (allAscii) {
			buf
		} else {
			charsetOpt match {
				case Some(charset) if DEF_CHARSET == charset => buf
				case Some(charset) => recode(buf, DEF_CHARSET, charset)
				case _ => tryDetectRecode(buf, DEF_CHARSET)
			}
		}
	}
}
