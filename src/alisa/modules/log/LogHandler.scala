package alisa.modules.log

import alisa.{IrcMessageEvent, IrcEventHandler}

final class LogHandler(lucene: LuceneService) extends IrcEventHandler[IrcMessageEvent] {

	def handle(event: IrcMessageEvent) = {
		lucene.addMessage(convertMessage(event))
		true
	}

	def convertMessage(event: IrcMessageEvent) =
		LuceneMessage(
			LuceneChannel(event.network.name, event.channel),
			System.currentTimeMillis,
			event.sender,
			event.login,
			event.hostname,
			event.message)
}
