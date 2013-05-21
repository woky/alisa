package alisa

import util.Random
import scala.Some
import Util._
import com.google.inject.AbstractModule
import com.google.inject.multibindings.Multibinder

object Hugs extends AbstractModule {

	def configure {
		Multibinder.newSetBinder(binder, classOf[ModuleHandlers]).addBinding.toInstance(HugsHandlers)
	}
}

object HugsHandlers extends ModuleHandlers {

	def randomReply(replies: Seq[String]) = replies(Random.nextInt(replies.length))

	val shyReplies = Seq(
		"%s, I-I do it only because %s ordered me to.",
		"%s, i-it's not like I feel anything to you.")

	def mkShyReply(recipient: String, commander: String) =
		randomReply(shyReplies).format(recipient, commander)

	val happyReplies = Seq(
		"%s, ( ´・‿-) ~ ♥",
		"%s, (  ^‿^) ~ ♥")

	def mkHappyReply(recipient: String) =
		randomReply(happyReplies).format(recipient)

	val empathyReplies = Seq(
		"%s, tell me everything that bothers you.",
		"%s, please, share all your pain with me.",
		"%s, I'm always here for you :-).",
		"%s, I won't let you down. Never.")

	def mkEmpathyReply(recipient: String) =
		randomReply(empathyReplies).format(recipient)


	override val command =
		Some(new IrcEventHandler[IrcCommandEvent] {
			def handle(event: IrcCommandEvent) =
				event match {
					case IrcCommandEvent(IrcEventContext(_, bot), channel, sender, _, _, "hug", args) => {
						val targets = mkArgs(args, Some(sender))
						val presentNicks = bot.getUsers(channel).map(_.getNick).toSet

						val replies = for (target <- targets) yield {
							if (sender.equalsIgnoreCase(target)
									|| "me".equalsIgnoreCase(target)
									|| "myself".equalsIgnoreCase(target))
								sender -> mkEmpathyReply(sender)
							else if (bot.getNick.equalsIgnoreCase(target)
									|| "you".equalsIgnoreCase("")
									|| "yourself".equalsIgnoreCase(target))
								sender -> mkHappyReply(sender)
							else if (presentNicks.contains(target))
								target -> mkShyReply(target, sender)
							else
								sender -> mkHappyReply(sender)
						}

						def hug(remaining: List[(String, String)], hugged: Set[String]) {
							remaining match {
								case (nick, reply) :: rest if !hugged.contains(nick) => {
									bot.sendAction(channel, s"hugs $nick")
									bot.sendMessage(channel, reply)
									hug(rest, hugged + nick)
								}
								case Nil =>
							}
						}

						hug(replies, Set())

						false
					}
					case _ => true
				}
		})

	override val action =
		Some(new IrcEventHandler[IrcActionEvent] {
			def handle(event: IrcActionEvent) =
				event match {
					case IrcActionEvent(IrcEventContext(_, bot), sender, _, _, channel, act) => {
						val actParts = mkArgs(act, limit = 2, regex = WS_SPLIT_REGEX)
						actParts match {
							case (cmd :: args :: Nil) if cmd.equals("hugs") => {
								val targets = mkArgs(args, Some(sender))
								val presentNicks = bot.getUsers(channel).map(_.getNick).toSet

								for (target <- targets) {
									val (targetNick, reply) =
										if (sender.equalsIgnoreCase(target)
												|| "himself".equalsIgnoreCase(target)
												|| "herself".equalsIgnoreCase(target))
											(sender, mkHappyReply(sender))
										else if (bot.getNick.equalsIgnoreCase(target))
											(sender, mkEmpathyReply(sender))
										else if (presentNicks.contains(target))
											(target, mkHappyReply(target))
										else
											(sender, mkHappyReply(sender))

									bot.sendAction(channel, s"hugs $targetNick")
									bot.sendMessage(channel, reply)
								}

								false

							}
							case _ => true
						}
					}
				}
		})
}
