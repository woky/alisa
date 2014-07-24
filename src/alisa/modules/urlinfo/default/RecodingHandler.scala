package alisa.modules.urlinfo.default

import org.xml.sax.Attributes
import java.nio.CharBuffer

/*
 * Observed Validator HTMLParser behaviour when reading a byte stream with unspecified charset:
 *  - find element in head
 *  - pass its latin1 decoded content to saxHandler
 *  - find <meta charset=".."> or <meta http-equiv="Content-Type" content="...">
 *  - pass correctly decoded content of all previous elements to saxHandler
 *  - pass meta element to saxHandler
 */

private final class RecodingHandler(buf: CharBuffer) extends TitleHandler(buf) {

	import TitleHandler._
	import StreamState._

	private var charsetFound = false
	private var allAscii = true // assume all non-Unicode encodings are ASCII compatible

	override def startElement(uri: String, name: String, qName: String, attrs: Attributes): Unit =
		streamState match {
			case SS_INIT if "html"  iceq name => streamState = SS_HTML
			case SS_INIT                      => break()
			case SS_HTML if "head"  iceq name => streamState = SS_HEAD
			case SS_HTML                      => break()
			case SS_HEAD if "title" iceq name =>
				streamState = SS_TITLE
				buf.position(0)
			case SS_HEAD if "meta"  iceq name =>
				val name2idx = (0 until attrs.getLength map { idx =>
					attrs.getLocalName(idx).toLowerCase -> idx
				}).toMap
				if (name2idx.contains("charset") || name2idx.get("http-equiv").exists(
						attrs.getValue(_).equalsIgnoreCase("content-type"))) {
					if (buf.position() == 0)
						charsetFound = true
					else
						break()
				}
			case _ =>
		}

	override protected def checkChar(c: Char): Unit =
		if (allAscii && c >= 0x80)
			allAscii = false

	override def endElement(uri: String, name: String, qName: String): Unit =
		if (streamState == SS_TITLE && ("title" iceq name)) {
			if (allAscii || charsetFound)
				break()
			else
				streamState = SS_HEAD
		} else if ((streamState == SS_HEAD && ("head" iceq name)) || (streamState != SS_HEAD))
			break()
}
