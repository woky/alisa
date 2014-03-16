package alisa.util

import java.nio.{BufferOverflowException, CharBuffer}
import scala.util.control.ControlThrowable

final class MessageBuffer(val underlying: CharBuffer) {

	lazy val overflowEx = new ControlThrowable {}

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
