package alisa

import org.jibble.pircbot.PircBot

case class IrcEventContext(network: String, bot: PircBot)

trait IrcEvent {

	def context: IrcEventContext
}

case class IrcMessageEvent(context: IrcEventContext,
                           channel: String,
                           sender: String,
                           login: String,
                           hostname: String,
                           message: String) extends IrcEvent

case class IrcCommandEvent(context: IrcEventContext,
                           channel: String,
                           sender: String,
                           login: String,
                           hostname: String,
                           command: String,
                           args: String) extends IrcEvent

case class IrcActionEvent(context: IrcEventContext,
                          sender: String,
                          login: String,
                          hostname: String,
                          target: String,
                          action: String) extends IrcEvent

