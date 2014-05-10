package alisa.modules

import alisa.{CmdHandler, IrcCommandEvent, SimpleModule}
import scala.util.Random

final class UnknownCmd extends SimpleModule("unknowncmd") with CmdHandler {

	def randomReply(replies: Seq[String]) = replies(Random.nextInt(replies.length))

	val shyReplies = Seq(
		"S-shut up, pervert!",
		"Shut up, p-pervert!")

	def handleCommand(event: IrcCommandEvent) {
		event.bot.sendMessage(event.channel, randomReply(shyReplies))
	}

	override def handles(cmd: String) = true
}
