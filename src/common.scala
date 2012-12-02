package alisa

import org.slf4j.LoggerFactory
import java.util.regex.Pattern
import java.io.InputStream

trait Service {

	def stop
}

trait Logger {

	private final val logger = LoggerFactory.getLogger(getClass)

	protected final def logDebug(msg: => String, ex: Throwable = null) {
		if (logger.isDebugEnabled)
			logger.debug(logMsg(msg), ex)
	}

	protected final def logInfo(msg: => String, ex: Throwable = null) {
		if (logger.isInfoEnabled)
			logger.info(logMsg(msg), ex)
	}

	protected final def logWarn(msg: => String, ex: Throwable = null) {
		if (logger.isWarnEnabled)
			logger.warn(logMsg(msg), ex)
	}

	protected final def logError(msg: => String, ex: Throwable = null) {
		if (logger.isErrorEnabled)
			logger.error(logMsg(msg), ex)
	}

	protected def logMsg(msg: => String): String = msg
}

class AsyncLoop(sleepTime: Long, proc: () => Unit) {

	private var shutdown = false

	private val monitor = new Object

	private val t = new Thread {

		start

		override def run {
			while (!shutdown) {
				monitor.synchronized {
					try {
						monitor.wait(10 * 60 * 1000)
					} catch {
						case _: InterruptedException => shutdown = true
					}

					proc()
				}
			}
		}
	}

	def stop {
		shutdown = true
		monitor.synchronized {
			monitor.notifyAll
		}
		t.join
	}
}

object Util {

	def prefixUnit(value: Long, suffix: String, si: Boolean = true): String = {
		val unit = if (si) 1000 else 1024
		if (value < unit) {
			value + " " + suffix
		} else {
			val exp = (Math.log(value) / Math.log(unit)).asInstanceOf[Int]
			val siPrefix = "kMGTPE".charAt(exp - 1)
			val prefix = if (si) siPrefix else (siPrefix.toUpper + "i")
			"%.1f %s%s".format(value / Math.pow(unit, exp), prefix, suffix)
		}
	}

	val DEFAULT_SPLIT_REGEX = Pattern.compile("\\s*[,; ]\\s*")
	val WS_SPLIT_REGEX = Pattern.compile("\\s+")
	val COLON_SPLIT_REGEX = Pattern.compile("\\s*[,;]\\s*")

	def mkArgs(trimmed: String, default: Option[String] = None, limit: Int = -1,
	           regex: Pattern = DEFAULT_SPLIT_REGEX): List[String] =
		regex.split(trimmed, limit).toList match {
			case "" :: Nil =>
				default match {
					case Some(s) => List(s)
					case None => Nil
				}
			case xs => xs
		}

	val PARAM_REGEX = Pattern.compile("(\\w+)(?:=(\\S+))?")

	def mkParams(trimmed: String) = {
		def iter(args: List[String], params: Map[String, Option[String]]): Map[String, Option[String]] =
			args match {
				case arg :: xs => {
					val matcher = PARAM_REGEX.matcher(arg)
					if (matcher.matches)
						matcher.group(2) match {
							case s: String => iter(xs, params + (matcher.group(1) -> Some(s)))
							case _ => iter(xs, params + (matcher.group(1) -> None)) // null
						}
					else
						iter(xs, params)
				}
				case Nil => params
			}

		iter(mkArgs(trimmed, None, regex = WS_SPLIT_REGEX), Map())
	}
}

final class LimitedInputStream(input: InputStream, private var limit: Long) extends InputStream {

	def read: Int = {
		if (limit <= 0)
			return -1

		val b = input.read
		limit -= 1
		b
	}

	override def read(b: Array[Byte]): Int = {
		if (limit <= 0)
			return -1

		val readLen = input.read(b)
		limit -= readLen
		readLen
	}

	override def read(b: Array[Byte], off: Int, len: Int): Int = {
		if (limit <= 0)
			return -1

		val readLen = input.read(b, off, len)
		limit -= readLen
		readLen
	}

	override def skip(n: Long): Long = {
		if (limit <= 0)
			return 0

		val readLen = skip(n)
		limit -= readLen
		readLen
	}

	override def available: Int = {
		if (limit <= 0)
			return 0

		input.available
	}

	override def close {
		input.close
	}

	override def mark(readlimit: Int) {
		input.mark(readlimit)
	}

	override def reset {
		input.reset
	}

	override def markSupported = input.markSupported
}
