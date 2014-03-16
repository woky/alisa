package alisa.modules.log

import alisa.{IrcChannelUser, IrcCommandEvent, SimpleCommandHandler}
import org.jibble.pircbot.PircBot
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
		val sender = event.user.user.nick

		def sendHelp() {
			bot.sendMessage(channel, s"$sender, usage: $command { link [<query>] | flush }")
		}

		if (IrcChannelUser.isAtLeastPrefix('+', event.user.modes)) {
			val args = parseArgs(event.args.decoded)
			args match {
				case subcmd :: subargs => subcmd match {
					case "link" => subargs match {
						case query :: _ => link(net, channel, sender, bot, query)
						case _ => link(net, channel, sender, bot, DEF_QUERY)
					}
					case "flush" => lucene.commit
					case _ => sendHelp()
				}
				case Nil => sendHelp()
			}
		} else {
			bot.sendMessage(sender, s"You must have at least voice in $channel to use this command.")
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
