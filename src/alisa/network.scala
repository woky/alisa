package alisa

import org.jibble.pircbot.{Colors, IrcException, PircBot}
import java.io.IOException
import java.util.regex.Pattern
import scala.Some
import java.util.concurrent.{Executors, ExecutorService, TimeUnit}
import java.nio.charset.Charset
import java.nio.CharBuffer
import com.ibm.icu.text.CharsetDetector
import java.nio.charset.spi.CharsetProvider
import scala.collection.JavaConversions._
import alisa.util.{Misc, Logger, ByteBufferInputStream}
import IrcEventHandlers._

object AlisaNetwork {

	final val CHARSET_NAME = "pircbot_charset_hack"
	final val INPUT_CHARSET = Charset.forName("ISO-8859-1")
	final val OUTPUT_CHARSET = Charset.forName("utf-8")
	final val CMD_PREFIXES = List(".", ";", "`")
	final val HL_DELIMS = List(':', ',')
}

final class AlisaNetwork(networkConf: NetworkConfig,
                         handlerMap: HandlerMap) extends PircBot with Logger {

	import AlisaNetwork._

	val network = IrcNetwork(networkConf.name, this)

	private var destroy = false
	private var executor: ExecutorService = _

	setName(networkConf.nick)
	setLogin(getName)
	setFinger(networkConf.finger)
	setVersion(getFinger)
	setEncoding(CHARSET_NAME)

	if (networkConf.servers.isEmpty)
		logWarn("No servers for network " + networkConf.name)
	else
		networkReconnect

	override def onMessage(channel: String, sender: String, login: String, hostname: String, rawMessage: String) {
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

	override def onAction(sender: String, login: String, hostname: String, target: String, rawAction: String) {
		val action = mkIrcText(rawAction)
		val event = IrcActionEvent(network, IrcUser(sender, login, hostname), target, action)
		handleEventAsync(event)
	}


	override def onPrivateMessage(sender: String, login: String, hostname: String, rawMessage: String) {
		val message = mkIrcText(rawMessage)
		val event = IrcPrivMsgEvent(network, IrcUser(sender, login, hostname), message)
		handleEventAsync(event)
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
