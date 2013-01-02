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

		/*
		 * TODO
		 * If some service fails to start, Guice will throw exception so we'll
		 * not be be able to access already started services that needs to be
		 * shut down. For example there's Lucene write.lock file that needs to
		 * be removed.
		 */
		val services = try {
			injector.getInstance(Key.get(new TypeLiteral[java.util.Set[Service]] {}))
		} catch {
			case e: Throwable => {
				logError("Error while creating services. Shutting down.", e)
				System.exit(1)
				return // make compiler happy
			}
		}

		sys.addShutdownHook {
			for (s <- services)
				try {
					s.stop
				} catch {
					case e: Throwable => logError(s"Error while stopping service $s", e)
				}
		}

		val handlers = {
			val userHandlers = injector.getInstance(Key.get(new TypeLiteral[java.util.Set[ModuleHandlers]] {}))
			config.handlers.foldRight(userHandlers.foldRight(DEFAULT_HANDLER_LISTS)(_ :: _))(_ :: _)
		}

		try {
			val nets = createNetworks(config, handlers)
			if (nets.isEmpty) {
				logError("No networks created. Shutting down.")
				System.exit(0)
			} else {
				sys.addShutdownHook(destroyNetworks(nets))
			}
		} catch {
			case e: Throwable => {
				logError("Error while creating networks. Shutting down.", e)
				System.exit(1)
			}
		}
	}

	def createNetworks(config: Config, handlers: IrcEventHandlerLists): List[AlisaNetwork] = {
		def iter(result: List[AlisaNetwork], netConfList: List[NetworkConfig]): List[AlisaNetwork] =
			netConfList match {
				case netConf :: xs =>
					try {
						iter(new AlisaNetwork(config.global, netConf, handlers) :: result, xs)
					} catch {
						case e: Throwable => {
							logError("Exception while creating network " + netConf, e)
							e match {
								case _: Exception => iter(result, xs)
								case _ => { // Error
									destroyNetworks(result)
									throw e
								}
							}

						}
					}
				case Nil => result
			}

		iter(Nil, config.networks.toList)
	}

	def destroyNetworks(networks: List[AlisaNetwork]) {
		for (net <- networks) {
			try {
				net.networkDisconnect
			} catch {
				case e: Throwable => {
					logError("Exception while destroying network " + net.networkConf, e)
				}
			}
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

		LogManager.getLogManager.reset
		SLF4JBridgeHandler.install
		JULLogger.getLogger(JULLogger.GLOBAL_LOGGER_NAME).setLevel(JULLevel.FINEST)
	}
}
