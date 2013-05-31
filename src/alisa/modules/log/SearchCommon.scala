package alisa.modules.log

import java.text.SimpleDateFormat
import java.util.Date

object SearchCommon {

	val DEFAULT_ID_TTL = 15 * 60

	val ID_KEY = "id"
	val QUERY_KEY = "query"
	val BYTIME_KEY = "sortByTime"
	val LIMIT_KEY = "limit"

	val DEF_QUERY = "*"
	val DEF_BYTIME = "true"
	val DEF_LIMIT = "50"

	private val DATE_FORMAT_TMPL = new SimpleDateFormat("yyyy-MM-dd HH:mm")

	def writeTextResults(results: Seq[LuceneStoredMessage], writeFunc: (String) => Unit) {
		val dateFormat = DATE_FORMAT_TMPL.clone.asInstanceOf[DATE_FORMAT_TMPL.type]
		def formatTextResult(msg: LuceneStoredMessage) = {
			val time = dateFormat.format(new Date(msg.stored.time))
			s"$time <${msg.stored.nick}> ${msg.stored.message}"
		}
		for (msg <- results)
			writeFunc(formatTextResult(msg))
	}
}
