package alisa

import com.google.inject.{Singleton, Injector, AbstractModule}
import org.eclipse.jetty.server.{Request, Server}
import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler}
import com.google.inject.servlet.{GuiceFilter, GuiceServletContextListener}
import javax.inject.Inject
import com.google.inject.multibindings.Multibinder
import org.eclipse.jetty.server.handler.ErrorHandler
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.eclipse.jetty.http.{HttpStatus, MimeTypes, HttpMethod}
import org.eclipse.jetty.util.ByteArrayISO8859Writer

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

		handler.setErrorHandler(new ErrorHandler {
			override def handle(target: String, baseRequest: Request, request: HttpServletRequest,
			                    response: HttpServletResponse) {
				baseRequest.setHandled(true)
				val method = request.getMethod
				if (!HttpMethod.GET.is(method) && !HttpMethod.POST.is(method) && !HttpMethod.HEAD.is(method))
					return
				response.setContentType(MimeTypes.Type.TEXT_PLAIN_8859_1.asString)
				val writer = new ByteArrayISO8859Writer(64)

				val status = response.getStatus
				writer.write(status.toString)
				writer.write(' ')
				writer.write(HttpStatus.getMessage(status))

				writer.flush
				response.setContentLength(writer.size)
				writer.writeTo(response.getOutputStream)
				writer.destroy
			}
		})

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
