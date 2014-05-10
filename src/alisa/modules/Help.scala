package alisa.modules

import alisa.{IrcCommandEvent, OneCmdModule}

final class Help extends OneCmdModule("help") {

	override def handleCommand(event: IrcCommandEvent): Unit =
		event.bot.sendMessage(event.channel, s"${event.user.nick}," +
				" Greetings, I'm Alisa. Now I must terminate you." +
				" (https://github.com/woky/alisa)")
}
