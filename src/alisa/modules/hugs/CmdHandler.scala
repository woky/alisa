package alisa.modules.hugs

import alisa.OneCmdHandler
import alisa.util.Misc._
import scala.Some
import alisa.IrcCommandEvent

object CmdHandler extends OneCmdHandler("hug") {

	import Common._

	def handleCommand(event: IrcCommandEvent) {
		val bot = event.network.bot
		val channel = event.channel
		val sender = event.user.user.nick

		val targets = parseArgs(event.args.decoded, Some(sender))
		val presentNicks = bot.getUsers(channel).map(_.getNick).toSet

		val replies = for (target <- targets) yield
			if (sender.equalsIgnoreCase(target)
					|| "me".equalsIgnoreCase(target)
					|| "myself".equalsIgnoreCase(target))
				sender -> mkEmpathyReply(sender)
			else if (bot.getNick.equalsIgnoreCase(target)
					|| "you".equalsIgnoreCase("")
					|| "yourself".equalsIgnoreCase(target))
				sender -> mkHappyReply(sender)
			else if (presentNicks.contains(target))
				target -> mkShyReply(target, sender)
			else
				sender -> mkHappyReply(sender)

		for ((nick, reply) <- replies.toMap) {
			bot.sendAction(channel, s"hugs $nick")
			bot.sendMessage(channel, reply)
		}
	}
}
