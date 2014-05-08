package alisa.modules

import alisa.{IrcCommandEvent, SimpleCmdModule}
import scala.util.Random

final class UnknownCmd extends SimpleCmdModule("unknowncmd", None) {

	def randomReply(replies: Seq[String]) = replies(Random.nextInt(replies.length))

	val shyReplies = Seq(
		"S-shut up, pervert!",
		"Shut up, p-pervert!")

	def handleCommand(event: IrcCommandEvent) {
		event.bot.sendMessage(event.channel, randomReply(shyReplies))
	}
}
