package alisa

import com.google.inject.{Singleton, Injector, AbstractModule}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler}
import com.google.inject.servlet.{GuiceFilter, GuiceServletContextListener}
import javax.inject.Inject
import com.google.inject.multibindings.Multibinder

object Jetty {

	def apply(httpPort: Int = 9080) = new Jetty(JettyConfig(httpPort))
}

final class Jetty(config: JettyConfig) extends AbstractModule {
	def configure() {
		bind(classOf[JettyConfig]).toInstance(config)
		bind(classOf[JettyService])
		Multibinder.newSetBinder(binder, classOf[Service]).addBinding.to(classOf[JettyService])
	}
}

final case class JettyConfig(httpPort: Int)

@Singleton
final class JettyService @Inject()(config: JettyConfig, injector: Injector) extends Service {

	val server = {
		val handler = new ServletContextHandler(ServletContextHandler.SESSIONS)
		handler.setContextPath("/")
		handler.addEventListener(new GuiceServletContextListener {
			override def getInjector = injector
		})
		handler.addFilter(classOf[GuiceFilter], "/*", null)
		handler.addServlet(classOf[DefaultServlet], "/")

		val server = new Server(config.httpPort)
		server.setHandler(handler)
		server.start

		server
	}

	def stop {
		server.stop
		server.join
	}
}
