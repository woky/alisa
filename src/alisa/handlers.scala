package alisa

object IrcEventHandlers {

	type HandlerMap = Map[Class[_ <: IrcEvent], List[IrcEventHandler]]

	def mkHandlerMap(list: List[IrcEventHandler]): HandlerMap =
		list.flatMap(h => h.handles.map(t => t -> h)).groupBy(_._1)
				.map(p => p._1 -> p._2.map(pp => pp._2).reverse).toMap // XXX ugly
}

trait IrcEventHandler {

	def handles: Set[Class[_ <: IrcEvent]]

	def handle: PartialFunction[IrcEvent, Boolean]
}

trait CmdHandler extends IrcEventHandler {

	final def handles = Set(classOf[IrcCommandEvent])

	final def handle = {
		case e: IrcCommandEvent =>
			if (handles(e.command)) {
				handleCommand(e)
				false
			} else {
				true
			}
	}

	def handles(cmd: String): Boolean

	def handleCommand(event: IrcCommandEvent)
}

abstract class OneCmdHandler(final val command: String) extends CmdHandler {

	override final def handles(cmd: String) = cmd == this.command
}
