package alisa.modules.urlinfo.default

import org.xml.sax.Attributes
import java.nio.CharBuffer

private final class DumbHandler(buf: CharBuffer) extends TitleHandler(buf) {

	import TitleHandler._
	import StreamState._

	override def startElement(uri: String, name: String, qName: String, attrs: Attributes): Unit =
		streamState match {
			case SS_INIT if "html"  iceq name => streamState = SS_HTML
			case SS_INIT                      => break()
			case SS_HTML if "head"  iceq name => streamState = SS_HEAD
			case SS_HTML                      => break()
			case SS_HEAD if "title" iceq name => streamState = SS_TITLE
			case _ =>
		}

	override def endElement(uri: String, name: String, qName: String): Unit =
		if ((streamState == SS_TITLE && ("title" iceq name))
				|| (streamState == SS_HEAD && ("head" iceq name))
				|| (streamState != SS_HEAD))
			break()
}
