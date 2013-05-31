package alisa.modules.hugs

import alisa._

final class Hugs extends Module with ModuleProvider with ModuleHandlers {

	override def handlers = Some(this)

	val name = "hugs"

	def create(params: Map[String, AnyRef]) = this

	override val command = Some(CmdHandler)

	override val action = Some(ActionHandler)
}
