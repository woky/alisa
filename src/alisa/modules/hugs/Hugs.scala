package alisa.modules.hugs

import alisa._

final class Hugs extends Module with ModuleProvider {

	val name = "hugs"

	def create(params: Map[String, AnyRef]) = this

	override def handler = Some(new IrcEventHandler {

		def handles = Set(classOf[IrcCommandEvent], classOf[IrcActionEvent])

		def handle(event: IrcEvent): Boolean = event match {
			case e: IrcCommandEvent => CmdHandler.handle(e)
			case e: IrcActionEvent => ActionHandler.handleAction(e)
		}
	})
}
