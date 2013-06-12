package alisa

import org.jibble.pircbot.{IrcException, PircBot}
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

object AlisaNetworkCommon {

	final val CHARSET_NAME = "pircbot_charset_hack"
	final val INPUT_CHARSET = Charset.forName("ISO-8859-1")
	final val OUTPUT_CHARSET = Charset.forName("utf-8")
}

final class AlisaNetwork(networkConf: NetworkConfig,
                         handlerLists: IrcEventHandlerLists) extends PircBot with Logger {

	import AlisaNetworkCommon._

	val cmdRegex: Pattern = Pattern.compile(s"^${networkConf.nick}\\s*[:, ]\\s*(\\S+)(?:\\s+(.+))?\\s*$$")
	val eventContext = IrcNetwork(networkConf.name, this)

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
		val message = decodeMessage(rawMessage)

		parseCommand(message) match {
			case Some((command, args)) => {
				val event = IrcCommandEvent(eventContext, channel, IrcUser(sender, login, hostname), command, args)
				handleEventAsync(event, handlerLists.command.list)
			}
			case None => {
				val event = IrcMessageEvent(eventContext, channel, IrcUser(sender, login, hostname), message)
				handleEventAsync(event, handlerLists.message.list)
			}
		}
	}

	override def onAction(sender: String, login: String, hostname: String, target: String, rawAction: String) {
		val action = decodeMessage(rawAction)

		val event = IrcActionEvent(eventContext, IrcUser(sender, login, hostname), target, action)
		handleEventAsync(event, handlerLists.action.list)
	}

	def parseCommand(message: String) = {
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

	def handleEventAsync[E <: IrcEvent](event: E, handlers: List[IrcEventHandler[E]]) {
		executor.submit(new Runnable {
			def run {
				try {
					handleEvent(event, handlers)
				} catch {
					case e: Throwable => logError("Exception while handling event " + event, e)
				}
			}
		})
	}

	def handleEvent[E <: IrcEvent](event: E, handlers: List[IrcEventHandler[E]]) {
		handlers match {
			case handler :: rest =>
				if (handler.handle(event))
					handleEvent(event, rest)
			case Nil =>
		}
	}

	override def onDisconnect {
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

object AlisaNetworkCharset extends Charset(AlisaNetworkCommon.CHARSET_NAME, Array()) {

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
