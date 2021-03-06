package alisa

import java.util.ServiceLoader
import scala.collection.JavaConverters._

abstract class Module {

	def stop {}

	def handler: Option[IrcEventHandler]
}

trait ModuleProvider {

	def name: String

	def create(params: Map[String, AnyRef]): Module
}

final class ModuleFactory {

	private val providerMap = ServiceLoader
			.load(classOf[ModuleProvider])
			.asScala
			.map(p => p.name.toLowerCase -> p)
			.toMap

	def create(name: String, params: Map[String, AnyRef]) = providerMap(name).create(params)
}

abstract class SimpleModule(final val name: String) extends Module with ModuleProvider
		with IrcEventHandler {

	override final def handler = Some(this)

	override final def create(params: Map[String, AnyRef]) = this
}

abstract class OneCmdModule(name: String, cmd: String) extends SimpleModule(name) with CmdHandler {

	def this(name: String) = this(name, name)

	override final def handles(cmd: String) = this.cmd == cmd
}
