package alisa.modules.log

import alisa.{IrcCommandEvent, SimpleCommandHandler}
import org.jibble.pircbot.PircBot
import scala.util.Random
import java.net.{InetSocketAddress, URLEncoder}
import alisa.util.Misc._

final class CmdHandler(allowedIds: AllowedIds, lucene: LuceneService, baseUrl: String)
		extends SimpleCommandHandler("log") {

	def this(allowedIds: AllowedIds, lucene: LuceneService, addr: InetSocketAddress) =
		this(allowedIds, lucene,
			 s"http://${addr.getAddress.getHostAddress}:${addr.getPort.toString}/")

	import SearchCommon._

	def handleCommand(event: IrcCommandEvent) {
		val bot = event.network.bot
		val net = event.network.name
		val channel = event.channel
		val sender = event.user.nick

		val args = mkArgs(event.args.decoded, Some(sender), 2)
		args match {
			case subcmd :: subargs => subcmd match {
				case "link" => subargs match {
					case query :: Nil => link(net, channel, sender, bot, query)
					case Nil => link(net, channel, sender, bot, DEF_QUERY)
					case _ => throw new AssertionError // shouldn't happen with split() with limit 2
				}
				case "flush" => lucene.commit
			}
			case Nil =>
				bot.sendMessage(channel, s"$sender, usage: $command { link [<query>] | flush }")
		}
	}

	def link(network: String, channel: String, sender: String, bot: PircBot, query: String) {
		val id = allowedIds.add(LuceneChannel(network, channel))

		val encQry = URLEncoder.encode(query, "utf-8")
		val link = s"$baseUrl$id?$QUERY_KEY=$encQry&$LIMIT_KEY=$DEF_LIMIT"
		val heading = s"You can browse logs for channel $channel in network $network for ${allowedIds.idTtl} seconds here:"

		lucene.commit
		bot.sendMessage(sender, heading)
		bot.sendMessage(sender, link)
	}
}
