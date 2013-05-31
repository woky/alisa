package alisa

trait IrcEventHandler[E <: IrcEvent] {

	def handle(event: E): Boolean
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
