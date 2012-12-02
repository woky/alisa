package alisa

import org.jibble.pircbot.PircBot

case class IrcEventContext(network: String, bot: PircBot)

trait IrcEvent {

	def context: IrcEventContext
}

trait IrcEventHandler[E <: IrcEvent] {

	def handle(event: E): Boolean
}

abstract class IrcFilteredEventHandler[E <: IrcEvent](handler: IrcEventHandler[E]) extends IrcEventHandler[E] {

	final def handle(event: E): Boolean =
		if (filter(event))
			handler.handle(event)
		else
			true

	def filter(event: E): Boolean = true
}

case class IrcMessageEvent(context: IrcEventContext,
                           channel: String,
                           sender: String,
                           login: String,
                           hostname: String,
                           message: String) extends IrcEvent

case class IrcCommandEvent(context: IrcEventContext,
                           channel: String,
                           sender: String,
                           login: String,
                           hostname: String,
                           command: String,
                           args: String) extends IrcEvent

case class IrcActionEvent(context: IrcEventContext,
                          sender: String,
                          login: String,
                          hostname: String,
                          target: String,
                          action: String) extends IrcEvent

trait ModuleHandlers {
	val message: Option[IrcEventHandler[IrcMessageEvent]] = None
	val command: Option[IrcEventHandler[IrcCommandEvent]] = None
	val action: Option[IrcEventHandler[IrcActionEvent]] = None
}

case class IrcEventHandlerList[E <: IrcEvent](list: List[IrcEventHandler[E]] = Nil) {

	def ::(x: Option[IrcEventHandler[E]]) =
		IrcEventHandlerList(
			list = x match {
				case Some(h) => h :: list
				case None => list
			})
}

case class IrcEventHandlerLists(message: IrcEventHandlerList[IrcMessageEvent] = IrcEventHandlerList(),
                                command: IrcEventHandlerList[IrcCommandEvent] = IrcEventHandlerList(),
                                action: IrcEventHandlerList[IrcActionEvent] = IrcEventHandlerList()) {

	def ::(x: ModuleHandlers) =
		IrcEventHandlerLists(message = x.message :: message,
		                     command = x.command :: command,
		                     action = x.action :: action)
}

abstract class SimpleCommandHandler(val command: String) extends IrcEventHandler[IrcCommandEvent] {

	final def handle(event: IrcCommandEvent) = {
		if (this.command == event.command) {
			handleCommand(event)

			false
		} else {
			true
		}
	}

	def handleCommand(event: IrcCommandEvent)
}

object UnknownCommandHandler extends IrcEventHandler[IrcCommandEvent] {

	def handle(event: IrcCommandEvent) = {
		event match {
			case IrcCommandEvent(IrcEventContext(_, bot), channel, sender, _, _, command, _) =>
				bot.sendMessage(channel, s"$sender, I don't know what does `$command' mean :(")
		}

		false
	}
}
