package alisa.util

import java.util.logging.{Level, Logger => JDKLogger}

trait Logger {

	// XXX maybe use just one global instance?
	private final val logger = JDKLogger.getLogger(getClass.getCanonicalName)

	protected final def logDebug(msg: => String, ex: Throwable = null) {
		if (logger.isLoggable(Level.FINE))
			logger.fine(logMsg(msg))
	}

	protected final def logInfo(msg: => String, ex: Throwable = null) {
		if (logger.isLoggable(Level.INFO))
			logger.info(logMsg(msg))
	}

	protected final def logWarn(msg: => String, ex: Throwable = null) {
		if (logger.isLoggable(Level.WARNING))
			logger.log(Level.WARNING, msg, ex)
	}

	protected final def logError(msg: => String, ex: Throwable = null) {
		if (logger.isLoggable(Level.SEVERE))
			logger.log(Level.SEVERE, msg, ex)
	}

	protected def logMsg(msg: => String): String = msg
}
