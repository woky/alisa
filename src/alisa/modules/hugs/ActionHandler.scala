package alisa.modules.hugs

import alisa.{IrcEvent, IrcEventHandler, IrcActionEvent}
import alisa.util.Misc._
import scala.Some

object ActionHandler extends IrcEventHandler {

	import Common._

	def handleAction(event: IrcActionEvent) = {
		val bot = event.bot
		val channel = event.target
		val sender = event.user.nick

		val actParts = parseArgs(event.action.decoded, limit = 2, regex = WS_SPLIT_REGEX)
		actParts match {
			case (cmd :: args :: Nil) if cmd.equals("hugs") =>
				val targets = parseArgs(args, Some(sender))
				val presentNicks = bot.getUsers(channel).map(_.getNick).toSet

				val replies = for (target <- targets) yield
					if (sender.equalsIgnoreCase(target)
							|| "himself".equalsIgnoreCase(target)
							|| "herself".equalsIgnoreCase(target))
						sender -> mkHappyReply(sender)
					else if (bot.getNick.equalsIgnoreCase(target))
						sender -> mkEmpathyReply(sender)
					else if (presentNicks.contains(target))
						target -> mkHappyReply(target)
					else
						sender -> mkHappyReply(sender)

				for ((nick, reply) <- replies.toMap) {
					bot.sendAction(channel, s"hugs $nick")
					bot.sendMessage(channel, reply)
				}

				false
			case _ => true
		}
	}

	def handles = Set(classOf[IrcActionEvent])

	def handle = {
		case e: IrcActionEvent => handleAction(e)
	}
}
