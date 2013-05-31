package alisa.modules.log

import com.sun.net.httpserver.HttpServer
import java.net.InetSocketAddress

final class SearchServer(bindAddr: InetSocketAddress, allowedIds: AllowedIds,
						 lucene: LuceneService) {

	private val httpServ = {
		val s = HttpServer.create(bindAddr, 0)
		s.createContext("/", new SearchHandler(allowedIds, lucene))
		s.setExecutor(null)
		s.start
		s
	}

	def stop {
		httpServ.stop(0)
	}
}
