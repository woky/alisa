package alisa.util

import java.util.logging.{Level, Logger => JDKLogger}
import Level._

trait Logger {

	// XXX maybe use just one global instance?
	private final val logger = JDKLogger.getLogger(getClass.getCanonicalName)

	protected final def logDebug(msg: => String, ex: Throwable = null) = _log(FINE, msg, ex)

	protected final def logInfo(msg: => String, ex: Throwable = null) = _log(INFO, msg, ex)

	protected final def logWarn(msg: => String, ex: Throwable = null) = _log(WARNING, msg, ex)

	protected final def logError(msg: => String, ex: Throwable = null) = _log(SEVERE, msg, ex)

	private def _log(level: Level, msg: => String, ex: Throwable = null): Unit =
		if (logger.isLoggable(level)) // needs to be checked before msg is evaluated
			logger.log(level, msg, ex)

	protected def logMsg(msg: => String): String = msg
}
