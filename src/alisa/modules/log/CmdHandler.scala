package alisa.modules.log

import alisa.{IrcChannelUser, IrcCommandEvent, OneCmdHandler}
import org.jibble.pircbot.PircBot
import java.net.{InetSocketAddress, URLEncoder}
import alisa.util.Misc._

final class LogCmdHandler(allowedIds: AllowedIds, lucene: LuceneService, baseUrl: String,
                       whitelist: Iterable[UserMatcher])
		extends OneCmdHandler("log") {

	def this(allowedIds: AllowedIds, lucene: LuceneService, addr: InetSocketAddress,
	         whitelist: Iterable[UserMatcher]) =
		this(allowedIds, lucene,
			 s"http://${addr.getAddress.getHostAddress}:${addr.getPort.toString}/", whitelist)

	import SearchCommon._

	def handleCommand(event: IrcCommandEvent) {
		val bot = event.bot
		val net = event.network.name
		val channel = event.channel
		val sender = event.user.nick

		def sendHelp() {
			bot.sendMessage(channel, s"$sender, usage: $command { link [<query>] | flush }")
		}

		if (authorized(event)) {
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

	private def authorized(event: IrcCommandEvent): Boolean =
		whitelist.exists(_(event.network.name, event.channel, event.user.user)) ||
				IrcChannelUser.isAtLeastPrefix('+', event.user.modes)

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
