package alisa.util

import java.nio.CharBuffer

object MessageBuffer {

	implicit def cb2mb(buf: CharBuffer) = new MessageBuffer(buf)
}

final class MessageBuffer(val underlying: CharBuffer) {

	def +=(c: Char): this.type = {
		underlying.append(c)
		this
	}

	def ++=(s: CharSequence): this.type = {
		underlying.append(s)
		this
	}

	def ++=(s: Any): this.type = ++=(s.toString)
}
