package alisa

import org.jibble.pircbot.PircBot

case class IrcNetwork(name: String, bot: PircBot)

trait IrcEvent {

	def network: IrcNetwork
}

case class IrcUser(nick: String, login: String, hostname: String)

case class IrcText(text: String, decoded: String)

case class IrcMessageEvent(network: IrcNetwork,
                           channel: String,
						   user: IrcUser,
                           message: IrcText) extends IrcEvent

case class IrcCommandEvent(network: IrcNetwork,
                           channel: String,
						   user: IrcUser,
                           command: String,
                           args: IrcText) extends IrcEvent

case class IrcActionEvent(network: IrcNetwork,
						  user: IrcUser,
                          target: String,
                          action: IrcText) extends IrcEvent

case class IrcPrivMsgEvent(network: IrcNetwork,
						   user: IrcUser,
						   message: IrcText) extends IrcEvent