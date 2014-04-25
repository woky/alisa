package alisa

import java.util.ServiceLoader
import scala.collection.JavaConversions._

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
			.map(p => p.name.toLowerCase -> p)
			.toMap

	def create(name: String, params: Map[String, AnyRef]) = providerMap(name).create(params)
}

abstract class SimpleModule(final val name: String) extends Module with ModuleProvider
		with IrcEventHandler {

	override final def handler = Some(this)

	override final def create(params: Map[String, AnyRef]) = this
}

abstract class SimpleCmdModule(name: String, optCmd: Option[String]) extends SimpleModule(name)
		with CmdHandler {

	def this(name: String) = this(name, Some(name))

	override final def handles(cmd: String) =
		this.optCmd match {
			case Some(handledCmd) => handledCmd == cmd
			case _ => true
		}
}
