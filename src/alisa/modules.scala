package alisa

import java.util.ServiceLoader
import scala.collection.JavaConversions._

trait ModuleHandlers {
	val message: Option[IrcEventHandler[IrcMessageEvent]] = None
	val command: Option[IrcEventHandler[IrcCommandEvent]] = None
	val action: Option[IrcEventHandler[IrcActionEvent]] = None
}

abstract class Module {

	def stop {}

	def handlers: Option[ModuleHandlers]
}

trait ModuleProvider {

	def name: String

	def create(params: Map[String, AnyRef]): Module
}

// TODO module sources
final class ModuleFactory {

	val providerMap = ServiceLoader
			.load(classOf[ModuleProvider])
			.map(p => p.name.toLowerCase -> p)
			.toMap

	def create(name: String, params: Map[String, AnyRef]) = providerMap(name).create(params)
}

sealed abstract class SimpleModule[T <: IrcEvent](val name: String)
		extends Module
		with ModuleProvider
		with ModuleHandlers
		with IrcEventHandler[T] {

	final override def handlers = Some(this)

	final def create(params: Map[String, AnyRef]) = this
}

abstract class SimpleMessageModule(name: String) extends SimpleModule[IrcMessageEvent](name) {

	final override val message = Some(this)
}

abstract class SimpleAnyCmdModule(name: String) extends SimpleModule[IrcCommandEvent](name) {

	final override val command = Some(this)
}

abstract class SimpleCmdModule(name: String, cmd: String) extends SimpleAnyCmdModule(name) {

	def this(name: String) = this(name, name)

	final def handle(event: IrcCommandEvent) = {
		if (cmd == event.command) {
			handleCommand(event)
			false
		} else {
			true
		}
	}

	def handleCommand(event: IrcCommandEvent)
}
