package alisa.modules

import alisa.{IrcCommandEvent, SimpleCmdModule}

final class Help extends SimpleCmdModule("help") {

	override def handleCommand(event: IrcCommandEvent): Unit =
		event.bot.sendMessage(event.channel, s"${event.user.nick}," +
				" Greetings, I'm Alisa. Now I must terminate you." +
				" (https://github.com/woky/alisa)")
}
