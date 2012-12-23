package alisa

import io.Source
import scala.tools.reflect.ToolBox
import scala.reflect.runtime._
import org.slf4j.bridge.SLF4JBridgeHandler
import scala.collection.JavaConversions._
import com.google.inject._
import multibindings.Multibinder
import sun.misc.{SignalHandler, Signal}
import org.slf4j.{Logger => SLF4JLogger, LoggerFactory}
import ch.qos.logback.classic.{Level, PatternLayout, LoggerContext, Logger => LBLogger}
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.classic.spi.ILoggingEvent
import joptsimple.OptionParser
import java.util.logging.{LogManager, Logger => JULLogger, Level => JULLevel}

object Alisa extends Logger {

	val DEFAULT_HANDLER_LISTS = IrcEventHandlerLists(command = IrcEventHandlerList(List(UnknownCommandHandler)))


	def main(args: Array[String]) {
		val termHandler = new SignalHandler {
			def handle(sig: Signal) {
				System.exit(0) // just override exit code to 0
			}
		}

		Signal.handle(new Signal("TERM"), termHandler)
		Signal.handle(new Signal("INT"), termHandler)

		val optParser = new OptionParser
		val debugOpt = optParser.accepts("d", "Debug log level").withOptionalArg.ofType(classOf[Boolean])

		val optSet = optParser.parse(args: _*)
		val newArgs = optSet.nonOptionArguments

		val verbose =
			if (optSet.has(debugOpt)) {
				if (optSet.hasArgument(debugOpt))
					Some(optSet.valueOf(debugOpt))
				else
					Some(true)
			} else {
				None
			}

		initLogging(verbose.getOrElse(false))

		var config =
			if (newArgs.length > 0)
				loadConfig(newArgs(0))
			else
				TestConfig

		if (verbose.isDefined)
			config = config.copy(global = config.global.copy(verbose = verbose.get))
		else if (config.global.verbose)
			initLogging(true)

		// empty sets
		val emptyMod = new AbstractModule {
			def configure {
				Multibinder.newSetBinder(binder, classOf[Service])
				Multibinder.newSetBinder(binder, classOf[ModuleHandlers])
			}
		}

		val injector = Guice.createInjector(emptyMod :: config.modules: _*)

		val services = injector.getInstance(Key.get(new TypeLiteral[java.util.Set[Service]] {}))
		sys.addShutdownHook {
			for (s <- services)
				try {
					s.stop
				} catch {
					case e: Exception => e.printStackTrace
				}
		}

		val handlers = {
			val userHandlers = injector.getInstance(Key.get(new TypeLiteral[java.util.Set[ModuleHandlers]] {}))
			config.handlers.foldRight(userHandlers.foldRight(DEFAULT_HANDLER_LISTS)(_ :: _))(_ :: _)
		}

		for (netConf <- config.networks) {
			val alisaNet = new AlisaNetwork(config.global, netConf, handlers)
			sys.addShutdownHook(alisaNet.networkDisconnect)
		}
	}

	def loadConfig(file: String): Config = {
		logInfo("Parsing config file")
		val confStr = "import alisa._\n" + Source.fromFile(file).mkString
		val tb = currentMirror.mkToolBox()
		val expr = tb.parse(confStr)
		val conf = tb.eval(expr).asInstanceOf[Config]
		logInfo("Done")

		conf
	}

	def initLogging(debug: Boolean) {
		val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
		lc.reset

		LogManager.getLogManager.reset
		SLF4JBridgeHandler.install
		JULLogger.getLogger(JULLogger.GLOBAL_LOGGER_NAME).setLevel(JULLevel.FINEST)

		val listener = new ch.qos.logback.classic.jul.LevelChangePropagator
		listener.setContext(lc)
		lc.addListener(listener)

		val layout = new PatternLayout
		layout.setContext(lc)

		val pattern =
			if (debug)
				"%d [%t] %-5p %c - %m%n"
			else
				"%p %c - %m%n"

		layout.setPattern(pattern)
		layout.start

		val appender = new ConsoleAppender[ILoggingEvent]
		appender.setContext(lc)
		appender.setLayout(layout)
		appender.start

		val rootLogger = LoggerFactory.getLogger(SLF4JLogger.ROOT_LOGGER_NAME).asInstanceOf[LBLogger]
		rootLogger.setLevel(Level.INFO)
		rootLogger.addAppender(appender)

		if (debug) {
			val ourLogger = LoggerFactory.getLogger(getClass.getPackage.getName).asInstanceOf[LBLogger]
			ourLogger.setLevel(Level.DEBUG)
		}

		lc.start
	}
}
