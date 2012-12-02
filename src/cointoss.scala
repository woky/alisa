package alisa

import util.Random
import com.google.inject.AbstractModule
import com.google.inject.multibindings.Multibinder

object Cointoss extends AbstractModule {
	def configure {
		Multibinder.newSetBinder(binder, classOf[ModuleHandlers]).addBinding.toInstance(CointossHandlers)
	}
}

object CointossHandlers extends ModuleHandlers {
	override val command = Some(new SimpleCommandHandler("cointoss") {
		def handleCommand(event: IrcCommandEvent) {
			val result =
				if (Random.nextBoolean)
					"heads"
				else
					"tails"
			event.context.bot.sendMessage(event.channel, s"${event.sender}, $result")
		}
	})
}
