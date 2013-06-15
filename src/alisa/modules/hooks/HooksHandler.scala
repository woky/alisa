package alisa.modules.hooks

import alisa._
import MsgType._
import java.util.regex.Pattern
import scala.util.Random

object HooksHandler {

	type ChannelHooks = Map[(String, String), List[ChannelHook]]
}

import HooksHandler._

final class HooksHandler(chanHooks: ChannelHooks) extends IrcEventHandler {

	private type IrcChanEvt = IrcEvent with IrcChannelEvent
	private type IrcChanUserEvt = IrcChanEvt with IrcUserEvent

	private case class HookPatt(hook: ChannelHook, patt: EvtPattern)

	private case class EvtHooks
		(msg:   List[HookPatt] = Nil,
		 cmd:   List[HookPatt] = Nil,
		 act:   List[HookPatt] = Nil,
		 join:  List[HookPatt] = Nil,
		 part:  List[HookPatt] = Nil)

	private def mkEvtHooks(hookList: List[ChannelHook]): EvtHooks = {
		val r = hookList.foldLeft(EvtHooks()) {
			(eh, h) => h.hookDef.patterns.foldLeft(eh) {
				(eh, p) => p.msgType match {
					case MSG  => eh.copy(msg  = HookPatt(h, p) :: eh.msg)
					case CMD  => eh.copy(cmd  = HookPatt(h, p) :: eh.cmd)
					case ACT  => eh.copy(act  = HookPatt(h, p) :: eh.act)
					case JOIN => eh.copy(join = HookPatt(h, p) :: eh.join)
					case PART => eh.copy(part = HookPatt(h, p) :: eh.part)
				}
			}
		}
		EvtHooks(msg    = r.msg.reverse,
		         cmd    = r.cmd.reverse,
		         act    = r.act.reverse,
		         join   = r.join.reverse,
		         part   = r.part.reverse)
	}

	private case class HookEvt(ie: IrcChanUserEvt, text: Option[String])

	private val chanEvtHooks = chanHooks.map { case (k, l) => (k -> mkEvtHooks(l)) }

	def handles: Set[Class[_ <: IrcEvent]] =
		Set(classOf[IrcMessageEvent],
		    classOf[IrcCommandEvent],
		    classOf[IrcActionEvent],
		    classOf[IrcJoinEvent],
		    classOf[IrcPartEvent])

	def handle = {
		case e: IrcMessageEvent =>  handleEvent(HookEvt(e, Some(e.message.decoded)), _.msg)
		case e: IrcCommandEvent =>  handleEvent(HookEvt(e, Some(e.args.decoded)), _.cmd)
		case e: IrcActionEvent =>   handleEvent(HookEvt(e, Some(e.action.decoded)), _.act)
		case e: IrcJoinEvent =>     handleEvent(HookEvt(e, None), _.join)
		case e: IrcPartEvent =>     handleEvent(HookEvt(e, None), _.part)
	}

	private def chanKey(e: IrcChanEvt) = (e.network.name, e.channel)

	private def hookPattList(e: IrcChanEvt, sel: (EvtHooks) => List[HookPatt])
	: Option[List[HookPatt]] =
		chanEvtHooks.get(chanKey(e)) match {
			case Some(x) => Some(sel(x))
			case _ => None
		}

	private def pattMatches(optPatt: Option[Pattern], text: String): Option[List[String]] =
		optPatt match {
			case Some(p) =>
				val m = p.matcher(text)
				if (!m.find)
					None
				else
					Some((0 to m.groupCount()).foldLeft(List[String]())(
						(l, n) => m.group(n) :: l)
					)
			case _ => Some(Nil)
		}

	private def addPattMatches(optPatt: Option[Pattern], text: String,
	                           matches: Option[List[String]]): Option[List[String]] =
		matches match {
			case Some(l1) =>
				pattMatches(optPatt, text) match {
					case Some(l2) => Some(l1 ++ l2)
					case _ => None
				}
			case _ => None
		}

	private def userMatches(optPatt: Option[UserPattern], user: IrcUser): Option[List[String]] =
		optPatt match {
			case Some(up) =>
				addPattMatches(up.hostname, user.hostname,
					addPattMatches(up.login, user.login,
						addPattMatches(up.nick, user.nick, Option(Nil))))
			case _ => Some(Nil)
		}

	private def sendReply(e: HookEvt, replies: Vector[List[Reply]], matches: List[String]) {
		val replyList = replies(Random.nextInt(replies.length))
		for (reply <- replyList) {
			val (bot, chan) = (e.ie.network.bot, e.ie.channel)

			// 1. network, 2. channel, 3. nick, 4. login, 5. hostname, 6. text, 7. time
			// 8-n. (userMatches*, textMatches*)
			val stdArgs = List(
				e.ie.network.name,
				chan,
				e.ie.user.nick,
				e.ie.user.login,
				e.ie.user.hostname,
				e.text.getOrElse(""),
				System.currentTimeMillis /* TODO use time from event */)
			val text = reply.text.format(stdArgs ++ matches: _*)

			reply.msgType match {
				case MSG  => bot.sendMessage(chan, text)
				case ACT  => bot.sendAction(chan, text)
			}
		}
	}

	private def handleEvent(e: HookEvt, sel: (EvtHooks) => List[HookPatt]) : Boolean =
		hookPattList(e.ie, sel) match {
			case Some(x) =>
				def iter(patts: List[HookPatt]): Boolean = {
					patts match {
						case hp :: xs =>
							val um = userMatches(hp.patt.user, e.ie.user)
							if (um.isDefined) {
								val matches = e.text match {
									case Some(t) => addPattMatches(hp.patt.text, t, um)
									case _ => um
								}
								matches match {
									case Some(m) =>
										val hDef = hp.hook.hookDef
										// TODO delay
										sendReply(e, hDef.replies, m)

										if (hDef.contHooks)
											iter(xs)
										else
											hDef.contHandlers
									case _ => iter(xs)
								}
							} else {
								iter(xs)
							}
						case _ => true
					}
				}
				iter(x)
			case _ => true
		}
}
