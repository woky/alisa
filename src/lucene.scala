package alisa

import org.apache.lucene.search._
import org.apache.lucene.index._
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.store.{FSDirectory, Directory}
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.util.Version
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper
import org.apache.lucene.analysis.core.{SimpleAnalyzer, KeywordAnalyzer}
import org.apache.lucene.analysis.standard.StandardAnalyzer
import scala.collection.JavaConversions._
import com.google.inject.{Inject, AbstractModule}
import com.google.inject.multibindings.Multibinder
import beans.BeanProperty
import org.apache.lucene.document._
import java.io.{Reader, File}
import javax.inject.Singleton
import org.apache.lucene.queryparser.classic.QueryParser.Operator
import org.apache.lucene.analysis.util.CharTokenizer
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents

object LuceneCommon {

	object FieldNames {
		final val TIME = "time"
		final val NET = "network"
		final val CHAN = "channel"
		final val NICK = "nick"
		final val USER = "user"
		final val HOST = "host"
		final val MSG = "message"
	}

	final val LUCENE_VERSION = Version.LUCENE_40
	final val DEFAULT_ANALYZER = new StandardAnalyzer(LUCENE_VERSION)
	final val FIELD_ANALYZERS_MAP = createFieldAnalyzerMap
	final lazy val DEFAULT_MESSAGE_ANALYZER = DEFAULT_ANALYZER

	def createAnalyzer(messageAnalyzer: Analyzer) = {
		val famap = FIELD_ANALYZERS_MAP + (FieldNames.MSG -> messageAnalyzer)
		new PerFieldAnalyzerWrapper(DEFAULT_ANALYZER, famap)
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

		Map(FieldNames.TIME -> kw,
			FieldNames.NET -> simple,
			FieldNames.CHAN -> simple,
			FieldNames.NICK -> simple,
			FieldNames.USER -> kw,
			FieldNames.HOST -> kw)
	}
}

object Lucene {

	def apply(dir: String, messageAnalyzer: Analyzer = LuceneCommon.DEFAULT_MESSAGE_ANALYZER) =
		new Lucene(LuceneConfig(FSDirectory.open(new File(dir)), messageAnalyzer))
}

final class Lucene(config: LuceneConfig) extends AbstractModule {

	def configure() {
		bind(classOf[LuceneConfig]).toInstance(config)
		bind(classOf[LuceneService])
		Multibinder.newSetBinder(binder, classOf[Service]).addBinding.to(classOf[LuceneService])
	}
}

final case class LuceneConfig(indexDir: Directory, messageAnalyzer: Analyzer)

final case class LuceneChannel(@BeanProperty network: String, @BeanProperty channel: String)

final case class LuceneMessage(@BeanProperty channel: LuceneChannel,
                               @BeanProperty time: Long,
                               @BeanProperty nick: String,
                               @BeanProperty user: String,
                               @BeanProperty host: String,
                               @BeanProperty message: String)

final case class LuceneStoredMessage(@BeanProperty id: Int, @BeanProperty stored: LuceneMessage)

final case class LuceneSearchParams(limit: Int, sortByTime: Boolean, reverse: Boolean = false)

@Singleton
final class LuceneService @Inject()(config: LuceneConfig) extends Service {

	import LuceneCommon._
	import LuceneCommon.FieldNames._

	val analyzer = createAnalyzer(config.messageAnalyzer)

	private val writer = {
		val wc = new IndexWriterConfig(LUCENE_VERSION, analyzer)
		val writer = new IndexWriter(config.indexDir, wc)
		writer
	}

	private var stopped = false
	private var reader: IndexReader = _
	private var searcher: IndexSearcher = _

	initReader

	private def initReader {
		reader = DirectoryReader.open(config.indexDir)
		searcher = new IndexSearcher(reader)
	}

	def stop {
		synchronized {
			if (!stopped) {
				writer.commit
				writer.close
				reader.close
				stopped = true
			}
		}
	}

	def commit {
		synchronized {
			if (!stopped) {
				writer.commit
				reader.close
				initReader
			}
		}
	}

	def getSearcher = synchronized {
		searcher
	}

	def createQueryParser = {
		val p = new QueryParser(LUCENE_VERSION, MSG, analyzer)
		p.setAllowLeadingWildcard(true)
		p.setDefaultOperator(Operator.AND)
		p
	}

	def createQuery(queryStr: String, channel: LuceneChannel) = {
		val query = new BooleanQuery
		query.add(createQueryParser.parse(queryStr), Occur.MUST)
		query.add(new TermQuery(new Term(NET, channel.network)), Occur.MUST)
		query.add(new TermQuery(new Term(CHAN, channel.channel)), Occur.MUST)
		query
	}

	def querySearch(query: Query, params: LuceneSearchParams) =
		if (params.sortByTime)
			getSearcher.search(query, params.limit,
				new Sort(new SortField(TIME, SortField.Type.INT, params.reverse)))
		else
			getSearcher.search(query, params.limit)

	def createResults(hits: Array[ScoreDoc]) =
		for (hit <- hits) yield {
			val doc = searcher.doc(hit.doc)
			new LuceneStoredMessage(hit.doc, LuceneMessage(LuceneChannel(doc.get(NET), doc.get(CHAN)),
				doc.getField(TIME).numericValue.intValue, doc.get(NICK), doc.get(USER), doc.get(HOST), doc.get(MSG)))
		}

	def search(queryStr: String, channel: LuceneChannel, params: LuceneSearchParams) = {
		val query = createQuery(queryStr, channel)
		synchronized {
			createResults(querySearch(query, params).scoreDocs)
		}
	}

	def addMessage(msg: LuceneMessage) {
		val doc = new Document
		doc.add(new LongField(TIME, msg.time, Field.Store.YES))
		doc.add(new StringField(NET, msg.channel.network, Field.Store.YES))
		doc.add(new StringField(CHAN, msg.channel.channel, Field.Store.YES))
		doc.add(new StringField(NICK, msg.nick, Field.Store.YES))
		doc.add(new StringField(USER, msg.user, Field.Store.YES))
		doc.add(new StringField(HOST, msg.host, Field.Store.YES))
		doc.add(new TextField(MSG, msg.message, Field.Store.YES))
		writer.addDocument(doc)
	}
}
