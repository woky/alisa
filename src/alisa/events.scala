package alisa

case class IrcNetwork(name: String, bot: AlisaNetwork)

case class IrcUser(nick: String, login: String, hostname: String)

object IrcChannelUser {

	final val PREFIX_TO_MODE = Map(
			'+' -> 'v',
			'%' -> 'h',
			'@' -> 'o',
			'&' -> 'a',
			'~' -> 'q')

	final val ORDERED_PREFIX_MODES = List('q', 'a', 'o', 'h', 'v')

	def isAtLeast(minDesired: Char, modes: Set[Char]): Boolean = {
		def iter(remaining: List[Char]): Boolean =
			remaining match {
				case desired :: xs =>
					if (modes(desired))
						true
					else if (desired != minDesired)
						iter(xs)
					else
						false
				case _ =>
					false
			}
		iter(ORDERED_PREFIX_MODES)
	}

	def isAtLeastPrefix(minDesired: Char, modes: Set[Char]): Boolean =
		isAtLeast(PREFIX_TO_MODE(minDesired), modes)
}

case class IrcChannelUser(user: IrcUser, modes: Set[Char]) {

	final def nick = user.nick
}

trait IrcEvent {

	def network: IrcNetwork

	//def time: Long // TODO

	final def bot = network.bot
}

trait IrcChannelEvent extends IrcEvent {

	def channel: String
}

trait IrcUserEvent extends IrcEvent {

	def user: IrcUser
}

trait IrcChannelUserEvent extends IrcEvent with IrcChannelEvent {

	def user: IrcChannelUser
}

case class IrcText(text: String, decoded: String)

case class IrcMessageEvent(network: IrcNetwork,
                           channel: String,
						   user: IrcChannelUser,
                           message: IrcText)
		extends IrcChannelUserEvent

case class IrcCommandEvent(network: IrcNetwork,
                           channel: String,
                           user: IrcChannelUser,
                           command: String,
                           args: IrcText)
		extends IrcChannelUserEvent

case class IrcActionEvent(network: IrcNetwork,
						  user: IrcChannelUser,
                          target: String,
                          action: IrcText)
		extends IrcChannelUserEvent {

	def channel = target
}

case class IrcPrivMsgEvent(network: IrcNetwork,
						   user: IrcUser,
						   message: IrcText)
		extends IrcUserEvent

case class IrcJoinEvent(network: IrcNetwork,
                        channel: String,
                        user: IrcChannelUser)
		extends IrcChannelUserEvent


case class IrcPartEvent(network: IrcNetwork,
                        channel: String,
                        user: IrcChannelUser)
		extends IrcChannelEvent
