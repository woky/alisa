package alisa.util

import java.io.InputStream

final class LimitedInputStream(input: InputStream, private var limit: Long)
		extends InputStream with Logger {

	override protected def logMsg(msg: => String) = s"$msg (limit = $limit)"

	def read: Int = synchronized {
		logDebug("read(), length = 1")
		if (limit <= 0)
			return -1

		val b = input.read
		limit -= 1
		b
	}

	override def read(b: Array[Byte], off: Int, len: Int): Int = synchronized {
		logDebug("read(), length = " + len)
		if (limit <= 0)
			return -1

		val readLen = input.read(b, off, len)
		limit -= readLen
		readLen
	}

	override def skip(n: Long): Long = synchronized {
		logDebug("skip(), length = " + n)
		if (limit <= 0)
			return 0

		val readLen = input.skip(n)
		limit -= readLen
		readLen
	}

	override def available: Int = synchronized {
		if (limit <= 0)
			return 0

		input.available
	}

	override def close {
		logDebug("close()")
		synchronized {
			input.close
		}
	}

	override def mark(readlimit: Int) {
		synchronized {
			input.mark(readlimit)
		}
	}

	override def reset {
		synchronized {
			input.reset
		}
	}

	override def markSupported = synchronized {
		input.markSupported
	}
}
