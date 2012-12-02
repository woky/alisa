package alisa

import io.Source
import scala.tools.reflect.ToolBox
import scala.reflect.runtime._
import org.slf4j.bridge.SLF4JBridgeHandler
import scala.collection.JavaConversions._
import com.google.inject._
import multibindings.Multibinder

object Alisa extends Logger {

	val DEFAULT_HANDLER_LISTS = IrcEventHandlerLists(command = IrcEventHandlerList(List(UnknownCommandHandler)))

	SLF4JBridgeHandler.install()

	def main(args: Array[String]) {
		val config = if (args.length > 0) {
			loadConfig(args(0))
		} else {
			TestConfig
		}

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
		val confStr = "import alisa.common._\n" + Source.fromFile(file).mkString
		val tb = currentMirror.mkToolBox()
		val expr = tb.parse(confStr)
		val conf = tb.eval(expr).asInstanceOf[Config]
		logInfo("Done")

		conf
	}

}
