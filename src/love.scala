package alisa

import org.jibble.pircbot.PircBot
import util.Random
import scala.Some
import Util._
import com.google.inject.AbstractModule
import com.google.inject.multibindings.Multibinder
import javax.inject.{Inject, Singleton}

object Love {

	lazy final val cute = apply(
		List("hug", "kiss"),
		Some(Seq("%s, I-I do it only because %s ordered me to.", "%s, I-It's not like I feel anything to you.")),
		Some(Seq("%s, ( ´・‿-) ~ ♥", "%s, (  ^‿^) ~ ♥")))

	def verbPairs(verbs: List[String]) = verbs.map(v => v -> Some(verbToPresentSimple(v) + " %s")).toMap

	def verbToPresentSimple(base: String) = {
		if (base.endsWith("y"))
			base.substring(0, base.length - 1) + "ies"
		else if (base.endsWith("s") || base.endsWith("z") || base.endsWith("ch") || base.endsWith("sh")
				|| base.endsWith("x") || base.endsWith("o"))
			base + "es"
		else
			base + "s"
	}

	def apply(): Love = cute

	def apply(verbs: List[String], cmdReplies: Option[Seq[String]], actReplies: Option[Seq[String]]): Love =
		new Love(LoveConfig(
			command = Some(
				LoveEventConfig(
					verbPairs(verbs),
					cmdReplies)),
			action = Some(
				LoveEventConfig(
					verbs.map(verbToPresentSimple(_)).map(v => v -> Some(v + " %s")).toMap,
					actReplies))))

	def commandNoAction(verbs: List[String], replies: Seq[String]): Love =
		new Love(LoveConfig(
			command = Some(
				LoveEventConfig(
					verbs.map(_ -> None).toMap,
					Some(replies))),
			action = None))
}

final class Love(config: LoveConfig) extends AbstractModule {

	def configure() {
		Multibinder.newSetBinder(binder, classOf[ModuleHandlers]).addBinding.toInstance(new LoveHandlers(config))
	}
}

final case class LoveConfig(command: Option[LoveEventConfig],
                            action: Option[LoveEventConfig])

final case class LoveEventConfig(verbs: Map[String, Option[String]],
                                 replies: Option[Seq[String]])

@Singleton
final class LoveHandlers @Inject()(config: LoveConfig) extends ModuleHandlers with Logger {

	if (config.action.isEmpty && config.command.isEmpty)
		logWarn("No events were configured")

	override val command = config.command match {
		case Some(evConfig) => {
			Some(new IrcEventHandler[IrcCommandEvent] {
				def handle(event: IrcCommandEvent) =
					event match {
						case IrcCommandEvent(IrcEventContext(_, bot), channel, sender, _, _, cmd, nicks) =>
							loveNicks(bot, channel, sender, true, cmd, nicks, evConfig)
					}
			})
		}
		case _ => None
	}

	override val action = config.action match {
		case Some(evConfig) => {
			Some(new IrcEventHandler[IrcActionEvent] {
				def handle(event: IrcActionEvent) =
					event match {
						case IrcActionEvent(IrcEventContext(_, bot), sender, _, _, channel, act) => {
							val args = mkArgs(act, limit = 2, regex = WS_SPLIT_REGEX)
							args match {
								case (cmd :: nicks :: Nil) =>
									loveNicks(bot, channel, sender, false, cmd, nicks, evConfig)
								case _ => true
							}
						}
					}
			})
		}
		case _ => None
	}

	def loveNicks(bot: PircBot, channel: String, sender: String, isCommand: Boolean, origVerb: String, args: String,
	              evConfig: LoveEventConfig): Boolean = {
		evConfig match {
			case LoveEventConfig(verbs, replies) =>
				verbs.get(origVerb) match {
					case Some(verb) => {
						val nicks = mkArgs(args, Some(sender))
						val presentNicks = bot.getUsers(channel).map(_.getNick).toSet

						for (nick <- nicks)
							if (sender.equals(nick) || "me".equals(nick)) {
								love(bot, channel, sender, None, verb, replies)
							} else if (isCommand) {
								if (!presentNicks.contains(nick))
									bot.sendMessage(channel, s"$sender, who's $nick?")
								else if (nick.equals(bot.getNick))
									bot.sendMessage(channel, s"$sender, I can't do that, sorry.")
								else
									love(bot, channel, nick, Some(sender), verb, replies)
							} else if (presentNicks.contains(nick) && !nick.equals(bot.getNick)) {
								love(bot, channel, nick, Some(sender), verb, replies)
							}

						false
					}
					case _ => true
				}
		}
	}

	def love(bot: PircBot, channel: String, rcpt: String, senderOpt: Option[String], optVerb: Option[String],
	         optReplies: Option[Seq[String]]) {
		val sender = senderOpt.getOrElse("you")

		optVerb match {
			case Some(verb) => bot.sendAction(channel, verb.format(rcpt, sender))
			case _ =>
		}

		optReplies match {
			case Some(msgs) => {
				val msg = msgs(Random.nextInt(msgs.length)).format(rcpt, sender)
				bot.sendMessage(channel, msg)
			}
			case _ =>
		}
	}
}
