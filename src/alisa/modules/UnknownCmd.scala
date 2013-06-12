package alisa.modules

import alisa.{IrcCommandEvent, SimpleAnyCmdModule}
import scala.util.Random

final class UnknownCmd extends SimpleAnyCmdModule("unknowncmd") {

	def randomReply(replies: Seq[String]) = replies(Random.nextInt(replies.length))

	val shyReplies = Seq(
		"S-shut up, pervert!",
		"Shut up, p-pervert!")

	def handle(event: IrcCommandEvent) = {
		event.network.bot.sendMessage(event.channel, randomReply(shyReplies))
		false
	}
}
