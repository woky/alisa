package alisa.modules.log

import org.apache.lucene.store.Directory
import java.net.InetSocketAddress
import alisa.{ModuleHandlers, Module}
import alisa.util.Logger

final class LogModule(indexDir: Directory, httpAddr: InetSocketAddress, idTtl: Int)
		extends Module with ModuleHandlers with Logger {

	private val allowedIds = new AllowedIds(idTtl)

	private val lucene = new LuceneService(indexDir)

	private val search =
		try {
			new SearchServer(httpAddr, allowedIds, lucene)
		} catch {
			case e: Exception => {
				lucene.stop
				throw e
			}
		}

	override def stop {
		try {
			lucene.stop
		} catch {
			case e: Exception => {
				logError("Failed to stop Lucene", e)
				throw e
			}
		} finally {
			search.stop
		}
	}

	override def handlers = Some(this)

	override val command = Some(new CmdHandler(allowedIds, lucene, httpAddr))

	override val message = Some(new LogHandler(lucene))
}
