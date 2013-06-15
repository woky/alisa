package alisa

import org.jibble.pircbot.{User, Colors, IrcException, PircBot}
import java.io.IOException
import java.util.regex.Pattern
import java.util.concurrent.{ConcurrentHashMap, Executors, ExecutorService, TimeUnit}
import java.nio.charset.Charset
import java.nio.CharBuffer
import com.ibm.icu.text.CharsetDetector
import java.nio.charset.spi.CharsetProvider
import alisa.util.{Misc, Logger, ByteBufferInputStream}
import IrcEventHandlers._
import scala.collection.JavaConversions._
import java.util.HashMap

private object AlisaNetworkCommon {

	final val CHARSET_NAME = "pircbot_charset_hack"
	final val INPUT_CHARSET = Charset.forName("ISO-8859-1")
	final val OUTPUT_CHARSET = Charset.forName("utf-8")
}

final class AlisaNetwork(networkConf: NetworkConfig,
                         handlerMap: HandlerMap) extends PircBot with Logger {

	import AlisaNetworkCommon._

	private val cmdRegex: Pattern = Pattern.compile(
		s"^${networkConf.nick}\\s*[:, ]\\s*(\\S+)(?:\\s+(.+))?\\s*$$")
	val network = IrcNetwork(networkConf.name, this)

	private var destroy = false
	private var executor: ExecutorService = _

	/*
	 * PircBot event methods (on*) run all in one thread. Only event handlers
	 * may access this map concurrently.
	 */
	private val chanUserModes = new HashMap[String, ConcurrentHashMap[String, Set[Char]]](
		networkConf.channels.length, 1f)
	for (c <- networkConf.channels)
		chanUserModes.put(c.name, new ConcurrentHashMap)

	setName(networkConf.nick)
	setLogin(getName)
	setFinger(networkConf.finger)
	setVersion(getFinger)
	setEncoding(CHARSET_NAME)

	if (networkConf.servers.isEmpty)
		logWarn("No servers for network " + networkConf.name)
	else
		networkReconnect

	protected override def onMessage(channel: String, sender: String, login: String, hostname: String, rawMessage: String) {
		parseCommand(rawMessage) match {
			case Some((command, rawArgs)) => {
				val args = mkIrcText(rawArgs)
				val event = IrcCommandEvent(network, channel, IrcUser(sender, login, hostname), command, args)
				handleEventAsync(event)
			}
			case None => {
				val msg = mkIrcText(rawMessage)
				val event = IrcMessageEvent(network, channel, IrcUser(sender, login, hostname), msg)
				handleEventAsync(event)
			}
		}
	}

	protected override def onAction(sender: String, login: String, hostname: String, target: String, rawAction: String) {
		val action = mkIrcText(rawAction)
		val event = IrcActionEvent(network, IrcUser(sender, login, hostname), target, action)
		handleEventAsync(event)
	}

	protected override def onPrivateMessage(sender: String, login: String, hostname: String, rawMessage: String) {
		val message = mkIrcText(rawMessage)
		val event = IrcPrivMsgEvent(network, IrcUser(sender, login, hostname), message)
		handleEventAsync(event)
	}

	protected override def onJoin(channel: String, sender: String, login: String, hostname: String) {
		assert(chanUserModes(channel).putIfAbsent(sender, Set.empty) == null)
		val event = IrcJoinEvent(network, channel, IrcUser(sender, login, hostname))
		handleEventAsync(event)
	}

	protected override def onPart(channel: String, sender: String, login: String, hostname: String) {
		assert(chanUserModes(channel).remove(sender) != null)
		val event = IrcPartEvent(network, channel, IrcUser(sender, login, hostname))
		handleEventAsync(event)
	}

	protected override def onVoice(channel: String, nick: String, login: String, hostname: String, recipient: String) {
		val oldModes = chanUserModes(channel).get(nick)
		assert(chanUserModes(channel).replace(nick, oldModes, oldModes + 'v'))
	}

	protected override def onDeVoice(channel: String, nick: String, login: String, hostname: String, recipient: String) {
		val oldModes = chanUserModes(channel).get(nick)
		assert(chanUserModes(channel).replace(nick, oldModes, oldModes - 'v'))
	}

	protected override def onOp(channel: String, nick: String, login: String, hostname: String, recipient: String) {
		val oldModes = chanUserModes(channel).get(nick)
		assert(chanUserModes(channel).replace(nick, oldModes, oldModes + 'o'))
	}

	protected override def onDeop(channel: String, nick: String, login: String, hostname: String, recipient: String) {
		val oldModes = chanUserModes(channel).get(nick)
		assert(chanUserModes(channel).replace(nick, oldModes, oldModes - 'o'))
	}

	protected override def onUserList(channel: String, users: Array[User]) {
		val chanMap = chanUserModes(channel)
		for (u <- users)
			chanMap.put(u.getNick, u.getPrefix match {
				case "" => Set.empty
				case s => s.toSet
			})
	}

	def userModes(channel: String, nick: String): Option[Set[Char]] = {
		chanUserModes.get(channel) match {
			case null => None
			case chanMap =>
				chanMap.get(nick) match {
					case null => None
					case c => Some(c)
				}
		}
	}

	private def parseCommand(message: String) = {
		val matcher = cmdRegex.matcher(message)
		if (matcher.matches) {
			val command = matcher.group(1)
			val args = matcher.group(2) match {
				case s: String => s
				case _ => "" // null
			}
			Some((command, args))
		} else {
			None
		}
	}

	private def handleEventAsync(event: IrcEvent) {
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

	private def handleEvent(event: IrcEvent) {
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

	protected override def onDisconnect {
		networkReconnect
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
		synchronized {
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

	private def decodeMessage(msg: String) = {
		logDebug("Decoding message \"" + Misc.escapeStringASCII(msg) + "\"")

		if (msg.forall(_ < 0x80)) {
			logDebug("Not decoding ASCII message")
			msg
		} else {
			val bbuf = INPUT_CHARSET.newEncoder.encode(CharBuffer.wrap(msg))
			val detector = new CharsetDetector
			detector.setText(new ByteBufferInputStream(bbuf.asReadOnlyBuffer))
			val csName = detector.detect.getName
			logDebug(s"Detected charset: $csName")

			val charset = Charset.forName(csName)
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

private object AlisaNetworkCharset extends Charset(AlisaNetworkCommon.CHARSET_NAME, Array()) {

	def contains(cs: Charset) = false // XXX ?

	def newDecoder = AlisaNetworkCommon.INPUT_CHARSET.newDecoder

	def newEncoder = AlisaNetworkCommon.OUTPUT_CHARSET.newEncoder
}

final class AlisaNetworkCharsetProvider extends CharsetProvider {

	val charsets: java.util.Iterator[Charset] = List(AlisaNetworkCharset).iterator

	def charsetForName(charsetName: String) =
		if (charsetName == AlisaNetworkCommon.CHARSET_NAME)
			AlisaNetworkCharset
		else
			null
}
