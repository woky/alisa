package alisa.modules.log

import org.apache.lucene.store.Directory
import org.apache.lucene.index._
import org.apache.lucene.search._
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.queryparser.classic.QueryParser.Operator
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.document._

final class LuceneService(indexDir: Directory) {

	import LuceneCommon._

	val analyzer = createAnalyzer(DEFAULT_MESSAGE_ANALYZER)

	private val writer = {
		val wc = new IndexWriterConfig(LUCENE_VERSION, analyzer)
		val writer = new IndexWriter(indexDir, wc)
		writer.commit
		writer
	}

	private var stopped = false
	private var reader: IndexReader = _
	private var searcher: IndexSearcher = _

	initReader

	private def initReader {
		reader = DirectoryReader.open(indexDir)
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
		val p = new QueryParser(LUCENE_VERSION, F_MSG, analyzer)
		p.setAllowLeadingWildcard(true)
		p.setDefaultOperator(Operator.AND)
		p
	}

	def createQuery(queryStr: String, channel: LuceneChannel) = {
		val query = new BooleanQuery
		query.add(createQueryParser.parse(queryStr), Occur.MUST)
		query.add(new TermQuery(new Term(F_NET, channel.network)), Occur.MUST)
		query.add(new TermQuery(new Term(F_CHAN, channel.channel)), Occur.MUST)
		query
	}

	def querySearch(query: Query, params: LuceneSearchParams) =
		if (params.sortByTime)
			getSearcher.search(query, params.limit, new Sort(new SortField(F_TIME, SortField.Type.LONG, true)))
		else
			getSearcher.search(query, params.limit)

	def createResults(hits: Array[ScoreDoc], reverse: Boolean) = {
		val range =
			if (reverse)
				hits.length - 1 to 0 by -1
			else
				0 until hits.length

		for (i <- range) yield {
			val doc = searcher.doc(hits(i).doc)
			new LuceneStoredMessage(hits(i).doc,
				LuceneMessage(LuceneChannel(
					doc.get(F_NET),
					doc.get(F_CHAN)),
					doc.getField(F_TIME).numericValue.longValue,
					doc.get(F_NICK),
					doc.get(F_USER),
					doc.get(F_HOST),
					doc.get(F_MSG)))
		}
	}

	def search(queryStr: String, channel: LuceneChannel, params: LuceneSearchParams) = {
		val query = createQuery(queryStr, channel)
		synchronized {
			createResults(querySearch(query, params).scoreDocs, params.sortByTime)
		}
	}



	def addMessage(msg: LuceneMessage) {
		val doc = new Document
		doc.add(new LongField(F_TIME, msg.time, Field.Store.YES))
		doc.add(new StringField(F_NET, msg.channel.network, Field.Store.YES))
		doc.add(new StringField(F_CHAN, msg.channel.channel, Field.Store.YES))
		doc.add(new StringField(F_NICK, msg.nick, Field.Store.YES))
		doc.add(new StringField(F_USER, msg.user, Field.Store.YES))
		doc.add(new StringField(F_HOST, msg.host, Field.Store.YES))
		doc.add(new TextField(F_MSG, msg.message, Field.Store.YES))
		writer.addDocument(doc)
	}
}
