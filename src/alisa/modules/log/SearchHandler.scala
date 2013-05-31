package alisa.modules.log

import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import java.io.{Writer, OutputStreamWriter}

final class SearchHandler(allowedIds: AllowedIds, lucene: LuceneService) extends HttpHandler {

	import SearchCommon._

	def parseQuery(q: String) =
		q.split("&").filter(!_.isEmpty).map(
			_.split("=", 2)).filter(!_.isEmpty).map({
			case Array(key, value) => key -> value
			case Array(key) => key -> ""
		}).toMap

	def handle(t: HttpExchange) {
		val path = t.getRequestURI.getPath
		val id = path.substring(1)

		allowedIds(id) match {
			case Some(chan) => {
				val (query, params) = try {
					val urlq = parseQuery(t.getRequestURI.getQuery)
					val query = urlq.getOrElse(QUERY_KEY, DEF_QUERY)
					val limit = urlq.getOrElse(LIMIT_KEY, DEF_LIMIT).toInt
					val byTime = urlq.getOrElse(BYTIME_KEY, DEF_BYTIME).toBoolean
					val p = LuceneSearchParams(limit, byTime)
					(query, p)
				} catch {
					case e: Exception => {
						e.printStackTrace()
						sendError(t, 400)
						return
					}
				}

				try {
					val r = lucene.search(query, chan, params)
					sendResults(t, r)
				} catch {
					case e: Exception => {
						// TODO log serious exceptions (eg. not query parser errors)
						sendError(t, 500)
						return
					}
				}
			}
			case _ => {
				sendError(t, 403)
				return
			}
		}
	}

	def sendResults(t: HttpExchange, results: Seq[LuceneStoredMessage]) {
		sendResponse(t, 200, w => writeTextResults(results, w.append(_).append("\n")))
	}

	def sendError(t: HttpExchange, code: Int) {
		sendResponse(t, code, _.write(code.toString))
	}

	def sendResponse(t: HttpExchange, code: Int, proc: (Writer) => Unit) {
		t.getResponseHeaders.set("Content-Type", "text/plain; charset=utf-8")
		t.sendResponseHeaders(code, 0)
		val wr = new OutputStreamWriter(t.getResponseBody)
		try {
			proc(wr)
		} finally {
			wr.close
		}
	}
}
