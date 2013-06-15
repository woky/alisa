package alisa.modules.hooks

import alisa.IrcUser
import java.util.regex.Pattern

object MsgType extends Enumeration {
	type MsgType = Value
	val MSG  = Value("Message")
	val CMD  = Value("Command")
	val ACT  = Value("Action")
	val JOIN = Value("Join")
	val PART = Value("Part")
}

import MsgType._

case class UserPattern(nick: Option[Pattern], login: Option[Pattern], hostname: Option[Pattern])

case class EvtPattern(msgType: MsgType, user: Option[UserPattern], text: Option[Pattern])

case class Reply(msgType: MsgType, text: String)

case class HookDef(patterns: List[EvtPattern], replies: Vector[List[Reply]], contHooks: Boolean,
                   contHandlers: Boolean, delay: Option[Long], randDelay: Option[Long])

case class ChannelHook(name: String, creator: IrcUser, created: Long, hookDef: HookDef)
