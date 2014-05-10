package alisa.modules

import alisa.{OneCmdModule, IrcCommandEvent}
import scala.util.Random

final class Cointoss extends OneCmdModule("cointoss") {

	def handleCommand(event: IrcCommandEvent) {
		val result = if (Random.nextBoolean) "heads" else "tails"
		event.bot.sendMessage(event.channel, s"${event.user.nick}, $result")
	}
}
