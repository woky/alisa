package alisa.modules.hugs

import scala.util.Random

object Common {

	val shyReplies = Seq(
		"%s, I-I only do it because %s ordered me to.",
		"%s, i-it's not like I feel anything to you.",
		"%s, you better consider it a privilege that I even converse with you!",
		"%s, b-be grateful! I may not do it next time.",
		"%s, it's not like I l-like you or anything stupid.")

	val happyReplies = Seq(
		"%s, ( ´・‿-) ~ ♥",
		"%s, (  ^‿^) ~ ♥")

	val empathyReplies = Seq(
		"%s, tell me everything that bothers you.",
		"%s, please, share all your pain with me.",
		"%s, I'm always here for you :-).",
		"%s, I won't let you down. Never.")


	def randomReply(replies: Seq[String]) = replies(Random.nextInt(replies.length))

	def mkShyReply(recipient: String, commander: String) =
		randomReply(shyReplies).format(recipient, commander)

	def mkHappyReply(recipient: String) =
		randomReply(happyReplies).format(recipient)

	def mkEmpathyReply(recipient: String) =
		randomReply(empathyReplies).format(recipient)
}
