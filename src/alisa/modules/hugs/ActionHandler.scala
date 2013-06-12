package alisa.modules.hugs

import alisa.IrcEventHandler
import alisa.util.Misc._
import alisa.IrcActionEvent
import scala.Some

object ActionHandler extends IrcEventHandler[IrcActionEvent] {

	import Common._

	def handle(event: IrcActionEvent) = {
		val bot = event.network.bot
		val channel = event.target
		val sender = event.user.nick

		val actParts = mkArgs(event.action, limit = 2, regex = WS_SPLIT_REGEX)
		actParts match {
			case (cmd :: args :: Nil) if cmd.equals("hugs") => {
				val targets = mkArgs(args, Some(sender))
				val presentNicks = bot.getUsers(channel).map(_.getNick).toSet

				for (target <- targets) {
					val (targetNick, reply) =
						if (sender.equalsIgnoreCase(target)
								|| "himself".equalsIgnoreCase(target)
								|| "herself".equalsIgnoreCase(target))
							(sender, mkHappyReply(sender))
						else if (bot.getNick.equalsIgnoreCase(target))
							(sender, mkEmpathyReply(sender))
						else if (presentNicks.contains(target))
							(target, mkHappyReply(target))
						else
							(sender, mkHappyReply(sender))

					bot.sendAction(channel, s"hugs $targetNick")
					bot.sendMessage(channel, reply)
				}

				false
			}
			case _ => true
		}
	}
}
