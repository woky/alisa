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

// TODO module sources
final class ModuleFactory {

	val providerMap = ServiceLoader
			.load(classOf[ModuleProvider])
			.map(p => p.name.toLowerCase -> p)
			.toMap

	def create(name: String, params: Map[String, AnyRef]) = providerMap(name).create(params)
}

sealed abstract class SimpleModule(final val name: String)
		extends Module
		with ModuleProvider
		with IrcEventHandler {

	final override def handler = Some(this)

	final def create(params: Map[String, AnyRef]) = this
}

/*
 * TODO remove following soon
 */

abstract class SimpleMessageModule(name: String) extends SimpleModule(name) {

	final def handles = Set(classOf[IrcMessageEvent])

	final def handle(event: IrcEvent) = event match {
		case e: IrcMessageEvent => handleMessage(e)
	}

	def handleMessage(event: IrcEvent): Boolean
}

abstract class SimpleAnyCmdModule(name: String) extends SimpleModule(name) {

	final def handles = Set(classOf[IrcCommandEvent])

	final def handle(event: IrcEvent) = event match {
		case e: IrcCommandEvent => handleCommand(e)
	}

	def handleCommand(event: IrcCommandEvent): Boolean
}

abstract class SimpleCmdModule(final val name: String, cmd: String)
		extends Module with ModuleProvider {

	def this(name: String) = this(name, name)

	final override def handler = Some(new SimpleCommandHandler(cmd) {
		def handleCommand(event: IrcCommandEvent) {
			SimpleCmdModule.this.handleCommand(event)
		}
	})

	final def create(params: Map[String, AnyRef]) = this

	def handleCommand(event: IrcCommandEvent)
}
