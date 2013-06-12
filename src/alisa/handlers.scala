package alisa

object IrcEventHandlers {

	type HandlerMap = Map[Class[_ <: IrcEvent], List[IrcEventHandler]]

	def mkHandlerMap(list: List[IrcEventHandler]): HandlerMap =
		list.flatMap(h => h.handles.map(t => (t -> h))).groupBy(_._1)
				.map(p => p._1 -> p._2.map(pp => pp._2).reverse).toMap // XXX ugly
}

trait IrcEventHandler {

	def handles: Set[Class[_ <: IrcEvent]]

	// TODO use PartialFunction?
	def handle(event: IrcEvent): Boolean
}

abstract class SimpleCommandHandler(val command: String) extends IrcEventHandler {

	def handles = Set(classOf[IrcCommandEvent])

	final def handle(event: IrcEvent) = event match {
		case e: IrcCommandEvent => {
			if (this.command == e.command) {
				handleCommand(e)
				false
			} else {
				true
			}
		}
	}

	def handleCommand(event: IrcCommandEvent)
}
