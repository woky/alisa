package alisa.util

import java.nio.ByteBuffer
import java.io.InputStream

final class ByteBufferInputStream(buf: ByteBuffer) extends InputStream {

	def read = synchronized {
		if (buf.hasRemaining)
			buf.get
		else
			-1
	}

	override def read(b: Array[Byte], off: Int, len: Int): Int = synchronized {
		if (buf.hasRemaining) {
			val readLen = Math.min(len, buf.remaining)
			buf.get(b, off, readLen)
			readLen
		} else {
			-1
		}
	}

	override def skip(n: Long) = synchronized {
		val toSkip = Math.min(n.toInt, buf.remaining)
		if (toSkip > 0)
			buf.position(buf.position() + toSkip)
		toSkip
	}

	override def available() = synchronized {
		buf.remaining
	}

	override def mark(readlimit: Int) {
		synchronized {
			buf.mark
		}
	}

	override def reset {
		synchronized {
			buf.reset
		}
	}

	override def markSupported = true
}
