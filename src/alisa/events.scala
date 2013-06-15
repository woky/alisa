package alisa

import org.jibble.pircbot.PircBot

case class IrcNetwork(name: String, bot: PircBot)

trait IrcEvent {

	def network: IrcNetwork
}

trait IrcChannelEvent { this: IrcEvent =>

	def channel: String
}

case class IrcUser(nick: String, login: String, hostname: String)

case class IrcText(text: String, decoded: String)

case class IrcMessageEvent(network: IrcNetwork,
                           channel: String,
						   user: IrcUser,
                           message: IrcText)
		extends IrcEvent with IrcChannelEvent

case class IrcCommandEvent(network: IrcNetwork,
                           channel: String,
						   user: IrcUser,
                           command: String,
                           args: IrcText)
		extends IrcEvent with IrcChannelEvent

case class IrcActionEvent(network: IrcNetwork,
						  user: IrcUser,
                          target: String,
                          action: IrcText)
		extends IrcEvent with IrcChannelEvent {

	def channel = target
}

case class IrcPrivMsgEvent(network: IrcNetwork,
						   user: IrcUser,
						   message: IrcText) extends IrcEvent

case class IrcJoinEvent(network: IrcNetwork,
                        channel: String,
                        user: IrcUser)
		extends IrcEvent with IrcChannelEvent


case class IrcPartEvent(network: IrcNetwork,
                        channel: String,
                        user: IrcUser)
		extends IrcEvent with IrcChannelEvent
