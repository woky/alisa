package alisa

import org.jibble.pircbot.PircBot
import util.Random
import scala.Some
import Util._
import com.google.inject.AbstractModule
import com.google.inject.multibindings.Multibinder
import javax.inject.{Inject, Singleton}

object Love {

	lazy final val cute = new LoveConfig(
		verbPairs(List("hug", "kiss")),
		Array("%s, I-I do it only because %s ordered me to.", "%s, I-It's not like I feel anything to you."),
		Some(Array("%s, ( ´・‿-) ~ ♥", "%s, (  ^‿^) ~ ♥")))

	def verbPairs(verbs: List[String]) = verbs.map(v => v -> verbToPresentSimple(v)).toMap

	def verbToPresentSimple(base: String) = {
		if (base.endsWith("y"))
			base.substring(0, base.length - 1) + "ies"
		else if (base.endsWith("s") || base.endsWith("z") || base.endsWith("ch") || base.endsWith("sh")
				|| base.endsWith("x") || base.endsWith("o"))
			base + "es"
		else
			base + "s"
	}

	def apply() = new Love(cute)
}

final class Love(config: LoveConfig) extends AbstractModule {

	def configure() {
		Multibinder.newSetBinder(binder, classOf[ModuleHandlers]).addBinding.toInstance(new LoveHandlers(config))
	}
}

final case class LoveConfig(psByVerb: Map[String, String],
                            commandMessages: Array[String],
                            actionMessages: Option[Array[String]])

@Singleton
final class LoveHandlers @Inject()(config: LoveConfig) extends ModuleHandlers {

	val psSet = config.psByVerb.map(_._2).toSet

	override val command = Some(new IrcEventHandler[IrcCommandEvent] {
		def handle(event: IrcCommandEvent) =
			event match {
				case IrcCommandEvent(IrcEventContext(_, bot), channel, sender, _, _, cmd, nicks) => {
					config.psByVerb.get(cmd) match {
						case Some(ps) => {
							loveNicks(bot, channel, sender, nicks, ps, config.commandMessages)
							false
						}
						case None => true
					}
				}
			}
	})

	override val action = Some(new IrcEventHandler[IrcActionEvent] {
		def handle(event: IrcActionEvent) =
			event match {
				case IrcActionEvent(IrcEventContext(_, bot), sender, _, _, target, act) => {
					val args = mkArgs(act, limit = 2, regex = WS_SPLIT_REGEX)
					args match {
						case (ps :: nicks :: Nil) => {
							val msgs = config.actionMessages.getOrElse(config.commandMessages)
							loveNicks(bot, target, sender, nicks, ps, msgs)
							false
						}
						case _ => true
					}
				}
			}
	})

	def loveNicks(bot: PircBot, channel: String, sender: String, args: String, ps: String, msgs: Array[String]) {
		val nicks = mkArgs(args, Some(sender))
		val presentNicks = bot.getUsers(channel).map(_.getNick).toSet

		for (nick <- nicks)
			if (sender.equals(nick) || "me".equals(nick))
				love(bot, channel, sender, None, ps, msgs)
			else if (presentNicks.contains(nick) && !nick.equals(bot.getNick))
				love(bot, channel, nick, Some(sender), ps, msgs)
	}

	def love(bot: PircBot, channel: String, rcpt: String, senderOpt: Option[String], ps: String, msgs: Array[String]) {
		val sender = senderOpt.getOrElse("you")
		val msg = msgs(Random.nextInt(msgs.length)).format(rcpt, sender)

		bot.sendAction(channel, ps + ' ' + rcpt)
		bot.sendMessage(channel, msg)
	}
}
