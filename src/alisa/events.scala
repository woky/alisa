package alisa

import org.jibble.pircbot.PircBot

case class IrcNetwork(name: String, bot: PircBot)

trait IrcEvent {

	def network: IrcNetwork
}

case class IrcUser(nick: String, login: String, hostname: String)

case class IrcMessageEvent(network: IrcNetwork,
                           channel: String,
						   user: IrcUser,
                           message: String) extends IrcEvent

case class IrcCommandEvent(network: IrcNetwork,
                           channel: String,
						   user: IrcUser,
                           command: String,
                           args: String) extends IrcEvent

case class IrcActionEvent(network: IrcNetwork,
						  user: IrcUser,
                          target: String,
                          action: String) extends IrcEvent
