package alisa.modules.log

import org.apache.lucene.store.Directory
import java.net.InetSocketAddress
import alisa._
import alisa.util.Logger

final class LogModule(indexDir: Directory, httpAddr: InetSocketAddress, idTtl: Int,
                      whitelist: Iterable[UserMatcher])
		extends Module with IrcEventHandler with Logger {

	private val allowedIds = new AllowedIds(idTtl, 16)

	private val lucene = new LuceneService(indexDir)

	private val search =
		try {
			new SearchServer(httpAddr, allowedIds, lucene)
		} catch {
			// TODO come up with something better
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

	override def handler = Some(this)

	def handles = Set(classOf[IrcMessageEvent], classOf[IrcCommandEvent])

	private val cmdHandler = new LogCmdHandler(allowedIds, lucene, httpAddr, whitelist)

	def handle = {
		case cmd: IrcCommandEvent => cmdHandler.handle(cmd)
		case msg: IrcMessageEvent => {
			lucene.addMessage(LuceneMessage(
				LuceneChannel(msg.network.name, msg.channel),
				System.currentTimeMillis,
				msg.user.nick,
				msg.user.user.login,
				msg.user.user.hostname,
				msg.message.decoded /* TODO store original text */))
			true
		}
	}
}
