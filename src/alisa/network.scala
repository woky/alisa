package alisa

import org.jibble.pircbot.{User, Colors, IrcException, PircBot}
import java.io.{UnsupportedEncodingException, IOException}
import java.util.concurrent.{Executors, ExecutorService, TimeUnit}
import java.nio.charset.{IllegalCharsetNameException, Charset}
import java.nio.CharBuffer
import com.ibm.icu.text.CharsetDetector
import java.nio.charset.spi.CharsetProvider
import alisa.util.{Misc, Logger, ByteBufferInputStream}
import IrcEventHandlers._
import scala.collection.JavaConversions._
import java.util.{Map => JMap, HashMap => JHashMap}

object AlisaNetwork {

	final val CHARSET_NAME = "pircbot_charset_hack"
	final val INPUT_CHARSET = Charset.forName("ISO-8859-1")
	final val OUTPUT_CHARSET = Charset.forName("utf-8")
	// TODO revert back when reply-mod is done
	final val CMD_PREFIXES = List.empty[String] //List(".", ";", "`")
	final val HL_DELIMS = List(':', ',')
}

final class AlisaNetwork(networkConf: NetworkConfig,
                         handlerMap: HandlerMap, verbose: Boolean) extends PircBot with Logger {

	import AlisaNetwork._

	val network = IrcNetwork(networkConf.name, this)

	private var destroy = false
	private var executor: ExecutorService = _

	private trait CachedUser
	private case class UserChanModes(chanModeMap: JMap[String, Set[Char]]) extends CachedUser
	private case class UserObjects(user: IrcUser, chanUserMap: JMap[String, IrcChannelUser]) extends CachedUser
	private val userMap = new JHashMap[String, CachedUser]()

	setName(networkConf.nick)
	setLogin(getName)
	setFinger(networkConf.finger)
	setVersion(getFinger)
	setEncoding(CHARSET_NAME)
	setVerbose(verbose)

	if (networkConf.servers.isEmpty)
		logWarn("No servers for network " + networkConf.name)
	else
		networkReconnect


	override protected def handleLine(line: String) {
		try {
			super.handleLine(line)
		} catch {
			case e: Exception => logError("Failed to handle line \"" + line + "\"", e)
		}
	}

	private def changeUserChanModes(channel: String, nick: String,
	                                change: (Set[Char]) => Set[Char]) {
		userMap.get(nick) match {
			case null =>
				val chanModeMap = new JHashMap[String, Set[Char]]
				chanModeMap(channel) = change(Set.empty)
				userMap(nick) = UserChanModes(chanModeMap)
			case UserChanModes(chanModeMap) =>
				chanModeMap(channel) = change(chanModeMap.getOrElse(channel, Set.empty))
			case UserObjects(user, chanUserMap) =>
				val oldModes = Option(chanUserMap.get(channel)).map(_.modes).getOrElse(Set.empty)
				chanUserMap(channel) = IrcChannelUser(user, change(oldModes))
		}
	}

	private def addUserChanMode(channel: String, nick: String, mode: Char) {
		changeUserChanModes(channel, nick, _ + mode)
	}

	private def delUserChanMode(channel: String, nick: String, mode: Char) {
		changeUserChanModes(channel, nick, _ - mode)
	}

	private def getChanUser(channel: String, nick: String, login: String,
	                        hostname: String): IrcChannelUser =
		userMap.get(nick) match {
			case cu@(null | UserChanModes(_)) =>
				val user = IrcUser(nick, login, hostname)
				val chanUser =
					cu match {
						case null =>
							val chanUser = IrcChannelUser(user, Set.empty)
							val chanUserMap = new JHashMap[String, IrcChannelUser](1)
							chanUserMap.put(channel, chanUser)
							userMap(nick) = UserObjects(user, chanUserMap)
							chanUser
						case UserChanModes(chanModeMap) =>
							val chanUserMap = chanModeMap.map {
								case (c, m) => c -> IrcChannelUser(user, m)
							}
							userMap(nick) = UserObjects(user, chanUserMap)
							chanUserMap.getOrElseUpdate(channel, IrcChannelUser(user, Set.empty))
					}
				chanUser
			case UserObjects(user, chanUserMap) =>
				chanUserMap.get(channel) match {
					case null =>
						val newChanUser = new IrcChannelUser(user, Set.empty)
						chanUserMap(channel) = newChanUser
						newChanUser
					case chanUser => chanUser
				}
		}

	private def getUser(nick: String, login: String, hostname: String) =
		userMap.get(nick) match {
			case tmpCu@(null | UserChanModes(_)) =>
				val user = IrcUser(nick, login, hostname)
				if (tmpCu != null) {
					val UserChanModes(chanModeMap) = tmpCu
					val chanUserMap = chanModeMap.map {
						case (c, m) => c -> IrcChannelUser(user, m)
					}
					userMap(nick) = UserObjects(user, chanUserMap)
				}
				user
			case UserObjects(user, _) => user
		}

	def prefixesToModes(prefixes: Iterable[Char], channel: String, nick: String): Set[Char] =
		prefixes.map(
			prefix => {
				val optMode = IrcChannelUser.PREFIX_TO_MODE.get(prefix)
				if (optMode.isEmpty)
					logWarn(s"Invalid user prefix `$prefix' " +
							s"(channel: $channel, nick: $nick})")
				optMode
			}
		).filter(_.isDefined).map(_.get).toSet

	override def onMessage(channel: String, sender: String, login: String, hostname: String, rawMessage: String) {
		val chanUser = getChanUser(channel, sender, login, hostname)
		parseCommand(rawMessage) match {
			case Some((command, rawArgs)) => {
				val args = mkIrcText(rawArgs)
				val event = IrcCommandEvent(network, channel, chanUser, command, args)
				handleEventAsync(event)
			}
			case None => {
				val msg = mkIrcText(rawMessage)
				val event = IrcMessageEvent(network, channel, chanUser, msg)
				handleEventAsync(event)
			}
		}
	}

	// TODO target should be only channel
	override def onAction(sender: String, login: String, hostname: String, target: String, rawAction: String) {
		val action = mkIrcText(rawAction)
		val chanUser = getChanUser(target, sender, login, hostname)
		val event = IrcActionEvent(network, chanUser, target, action)
		handleEventAsync(event)
	}

	override def onPrivateMessage(sender: String, login: String, hostname: String, rawMessage: String) {
		val message = mkIrcText(rawMessage)
		val event = IrcPrivMsgEvent(network, getUser(sender, login, hostname), message)
		handleEventAsync(event)
	}

	override def onJoin(channel: String, nick: String, login: String, hostname: String) {
		val chanUser = getChanUser(channel, nick, login, hostname)
		val event = IrcJoinEvent(network, channel, chanUser)
		handleEventAsync(event)
	}

	override def onPart(channel: String, nick: String, login: String, hostname: String) {
		val chanUser =
			userMap.get(nick) match {
				case cu@(null | UserChanModes(_)) =>
					val user = IrcUser(nick, login, hostname)
					val chanModes =
						cu match {
							case null => Set.empty[Char]
							case UserChanModes(chanModeMap) =>
								val chanModes = chanModeMap.remove(channel)
								if (chanModeMap.isEmpty)
									userMap.remove(nick)
								if (chanModes == null) {
									logWarn(s"Unknown user `$nick' parted `$channel'")
									Set.empty[Char]
								} else {
									chanModes
								}
						}
					IrcChannelUser(user, chanModes)
				case UserObjects(user, chanUserMap) =>
					val chanUser = chanUserMap.remove(channel)
					if (chanUserMap.isEmpty)
						userMap.remove(nick)
					if (chanUser == null) {
						logWarn(s"Unknown user `$nick' parted `$channel'")
						IrcChannelUser(user, Set.empty)
					} else {
						chanUser
					}
			}

		val event = IrcPartEvent(network, channel, chanUser)
		handleEventAsync(event)
	}

	override def onQuit(nick: String, login: String, hostname: String, reason: String) {
		val user =
			userMap.remove(nick) match {
				case UserObjects(user, _) => user
				case _ => IrcUser(nick, login, hostname)
			}

		// TODO
		//val event = IrcQuitEvent(network, user, reason)
		//handleEventAsync(event)
	}

	override def onVoice(channel: String, nick: String, login: String, hostname: String, recipient: String) {
		addUserChanMode(channel, recipient, 'v')
	}

	override def onDeVoice(channel: String, nick: String, login: String, hostname: String, recipient: String) {
		delUserChanMode(channel, recipient, 'v')
	}

	override def onOp(channel: String, nick: String, login: String, hostname: String, recipient: String) {
		addUserChanMode(channel, recipient, 'o')
	}

	override def onDeop(channel: String, nick: String, login: String, hostname: String, recipient: String) {
		delUserChanMode(channel, recipient, 'o')
	}

	override def onUserList(channel: String, users: Array[User]) {
		for (u <- users) {
				val nick = u.getNick
				val chanModes = prefixesToModes(u.getPrefix, channel, nick)
				changeUserChanModes(channel, nick, _ => chanModes)
		}
	}

	def parseCommand(msg: String): Option[(String, String)] = {
		def mkArgs(line: String) = {
			val (cmd :: argList) = Misc.mkArgs(line, None, 2, Misc.WS_SPLIT_REGEX)
			val args = argList match {
				case s :: Nil => s
				case _ => ""
			}
			Some(cmd, args)
		}

		def prefixed(p: String) = (msg.startsWith(p) && (msg.length > p.length
				&& !Character.isWhitespace(msg(p.length))))
		CMD_PREFIXES.find(prefixed) match {
			case Some(prefix) => mkArgs(msg.substring(prefix.length))
			case _ =>
				def hlDelim(c: Char) = Character.isWhitespace(c) || HL_DELIMS.contains(c)
				val n = getNick
				if (msg.startsWith(n) && msg.length > n.length && hlDelim(msg(n.length))) {
					msg.indexWhere(!Character.isWhitespace(_), n.length + 1) match {
						case -1 => None
						case start => mkArgs(msg.substring(start))
					}
				} else {
					None
				}
		}
	}

	def handleEventAsync(event: IrcEvent) {
		executor.submit(new Runnable {
			def run {
				try {
					handleEvent(event)
				} catch {
					case e: Throwable => logError("Exception while handling event " + event, e)
				}
			}
		})
	}

	def handleEvent(event: IrcEvent) {
		def iterHandlers(handlers: List[IrcEventHandler]) {
			handlers match {
				case h :: xs =>
					if (h.handle(event))
						iterHandlers(xs)
				case Nil =>
			}
		}

		handlerMap.get(event.getClass) match {
			case Some(handlers) => iterHandlers(handlers)
			case None =>
		}
	}

	override def onDisconnect {
		synchronized {
			// XXX hotfix
			if (!destroy) {
				Thread.sleep(30000)
				networkReconnect
			}
		}
	}

	def networkDisconnect {
		synchronized {
			shutdownExecutor

			disconnect
			dispose

			destroy = true
			notifyAll
		}
	}

	private def networkReconnect {
		logDebug("Reconnecting")

		shutdownExecutor
		executor = Executors.newSingleThreadExecutor

		while (!destroy) {
			for {
				server <- networkConf.servers
				_ <- 0 until server.reconnTries
			} {
				try {
					logInfo("Connecting to " + server)
					connect(server.host, server.port)
					logInfo("Connected")

					for (chan <- networkConf.channels)
						joinChannel(chan.name)

					return
				} catch {
					case e@(_: IOException | _: IrcException) => {
						logError("Could not connect to " + server, e)
						AlisaNetwork.this.wait(server.reconnDelay)
						if (destroy)
							return
					}
				}
			}
		}
	}

	private def shutdownExecutor {
		logDebug("Halting background tasks")
		if (executor != null && !executor.isTerminated) {
			executor.shutdownNow
			executor.awaitTermination(Long.MaxValue, TimeUnit.MILLISECONDS)
			executor = null
		}
		logDebug("Done")
	}

	override protected def logMsg(msg: => String) = "[" + networkConf.name + "] " + msg

	private def mkIrcText(orig: String) =
		IrcText(orig, decodeMessage(Colors.removeFormattingAndColors(orig)))

	def decodeMessage(msg: String) = {
		logDebug("Decoding message \"" + Misc.escapeStringASCII(msg) + "\"")

		if (msg.forall(_ < 0x80)) {
			logDebug("Not decoding ASCII message")
			msg
		} else {
			val bbuf = INPUT_CHARSET.newEncoder.encode(CharBuffer.wrap(msg))
			val detector = new CharsetDetector
			detector.setText(new ByteBufferInputStream(bbuf.asReadOnlyBuffer))

			val charset = {
				val csMatch = detector.detect
				if (csMatch != null) {
					val csName = csMatch.getName
					logDebug(s"Detected charset: $csName")
					try {
						Charset.forName(csMatch.getName)
					} catch {
						case e@(_: UnsupportedEncodingException |
								_: IllegalCharsetNameException) =>
							logWarn("Invalid charset detected (" + csName + "). Message: \""
									+ Misc.escapeStringASCII(msg) + "\"", e)
							INPUT_CHARSET
					}
				} else {
					logDebug("No charset detected")
					INPUT_CHARSET
				}
			}

			if (INPUT_CHARSET == charset) {
				logDebug("Not decoding")
				msg
			} else {
				val newMsg = charset.decode(bbuf).toString
				logDebug("Decoded message \"" + newMsg + "\"")
				newMsg
			}
		}
	}
}

object AlisaNetworkCharset extends Charset(AlisaNetwork.CHARSET_NAME, Array()) {

	def contains(cs: Charset) = false // XXX ?

	def newDecoder = AlisaNetwork.INPUT_CHARSET.newDecoder

	def newEncoder = AlisaNetwork.OUTPUT_CHARSET.newEncoder
}

final class AlisaNetworkCharsetProvider extends CharsetProvider {

	val charsets: java.util.Iterator[Charset] = List(AlisaNetworkCharset).iterator

	def charsetForName(charsetName: String) =
		if (charsetName == AlisaNetwork.CHARSET_NAME)
			AlisaNetworkCharset
		else
			null
}
