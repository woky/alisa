package alisa.modules.urlinfo.default

import org.xml.sax.helpers.DefaultHandler
import java.io._
import alisa.util.MessageBuffer
import MessageBuffer._
import scala.util.control.ControlThrowable
import java.nio.CharBuffer
import alisa.modules.urlinfo.Common

private object TitleHandler {

	object StreamState extends Enumeration {
		type State = Value
		val SS_INIT, SS_HTML, SS_HEAD, SS_TITLE = Value
	}

	private object TitleState extends Enumeration {
		type TitleState = Value
		val TS_INIT, TS_TEXT, TS_SPACE = Value
	}

	implicit final class CIString(orig: String) {
		def iceq(other: String) = orig.equalsIgnoreCase(other)
	}
}

private abstract class TitleHandler(buf: CharBuffer) extends DefaultHandler {

	import TitleHandler._
	import StreamState._
	import TitleState._

	final val breakEx = new IOException with ControlThrowable

	protected final var streamState = SS_INIT
	private final var titleState = TS_INIT

	protected final def break(): Nothing = throw breakEx
	protected def checkChar(c: Char) {}

	def title: CharSequence

	override def startDocument: Unit = buf.position(0)

	override def characters(ch: Array[Char], start: Int, length: Int): Unit =
		if (streamState == SS_TITLE) {
			for (i <- 0 until Math.min(length, buf.remaining())) {
				val c = ch(start + i)
				if (Character.isWhitespace(c)) {
					if (titleState == TS_TEXT)
						titleState = TS_SPACE
				} else {
					checkChar(c)
					if (titleState == TS_INIT) {
						buf ++= Common.TITLE_PREFIX
						titleState = TS_TEXT
					} else if (titleState == TS_SPACE) {
						buf += ' '
						titleState = TS_TEXT
					}
					buf += c
				}
			}
			if (!buf.hasRemaining)
				break()
		}
}
