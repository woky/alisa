package alisa.modules.log

import org.apache.lucene.util.Version
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper
import org.apache.lucene.analysis.core.KeywordAnalyzer
import java.io.Reader
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.util.CharTokenizer
import scala.collection.JavaConverters._

object LuceneCommon {

	final val F_TIME = "time"
	final val F_NET = "network"
	final val F_CHAN = "channel"
	final val F_NICK = "nick"
	final val F_USER = "user"
	final val F_HOST = "host"
	final val F_MSG = "message"

	final val LUCENE_VERSION = Version.LUCENE_40
	final val DEFAULT_ANALYZER = new StandardAnalyzer(LUCENE_VERSION)
	final val FIELD_ANALYZERS_MAP = createFieldAnalyzerMap
	final lazy val DEFAULT_MESSAGE_ANALYZER = DEFAULT_ANALYZER

	def createAnalyzer(messageAnalyzer: Analyzer) = {
		val famap = FIELD_ANALYZERS_MAP + (F_MSG -> messageAnalyzer)
		new PerFieldAnalyzerWrapper(DEFAULT_ANALYZER, famap.asJava)
	}

	def createFieldAnalyzerMap = {
		val kw = new KeywordAnalyzer
		val simple: Analyzer = new Analyzer {
			def createComponents(fieldName: String, reader: Reader) =
				new TokenStreamComponents(new CharTokenizer(LUCENE_VERSION, reader) {
					def isTokenChar(c: Int) = true

					override def normalize(c: Int) = Character.toLowerCase(c)
				})
		}

		Map(F_TIME -> kw,
			F_NET -> simple,
			F_CHAN -> simple,
			F_NICK -> simple,
			F_USER -> kw,
			F_HOST -> kw)
	}
}

final case class LuceneChannel(network: String, channel: String)

final case class LuceneMessage(channel: LuceneChannel,
							   time: Long,
							   nick: String,
							   user: String,
							   host: String,
							   message: String)

final case class LuceneStoredMessage(id: Int, stored: LuceneMessage)

final case class LuceneSearchParams(limit: Int, sortByTime: Boolean)
