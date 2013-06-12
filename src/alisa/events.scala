package alisa

import org.jibble.pircbot.PircBot

case class IrcNetwork(name: String, bot: PircBot)

trait IrcEvent {

	def network: IrcNetwork
}

case class IrcMessageEvent(network: IrcNetwork,
                           channel: String,
                           sender: String,
                           login: String,
                           hostname: String,
                           message: String) extends IrcEvent

case class IrcCommandEvent(network: IrcNetwork,
                           channel: String,
                           sender: String,
                           login: String,
                           hostname: String,
                           command: String,
                           args: String) extends IrcEvent

case class IrcActionEvent(network: IrcNetwork,
                          sender: String,
                          login: String,
                          hostname: String,
                          target: String,
                          action: String) extends IrcEvent
