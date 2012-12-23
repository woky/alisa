package alisa

import org.jibble.pircbot.{IrcException, PircBot}
import java.io.IOException
import java.util.regex.Pattern
import scala.Some
import java.util.concurrent.{Executors, ExecutorService, TimeUnit}
import java.nio.charset.Charset
import java.nio.CharBuffer
import com.ibm.icu.text.CharsetDetector

object AlisaNetworkCommon {

	final val DEFAULT_CHARSET_NAME = "latin1"
	final val DEFAULT_CHARSET = Charset.forName(DEFAULT_CHARSET_NAME)
}

class AlisaNetwork(val globalConf: GlobalConfig,
                   val networkConf: NetworkConfig,
                   val handlerLists: IrcEventHandlerLists) extends PircBot with Logger {

	import AlisaNetworkCommon._

	val cmdRegex: Pattern = Pattern.compile(s"^${networkConf.nick}\\s*[:, ]\\s*(\\S+)(?:\\s+(.+))?\\s*$$")
	val eventContext = IrcEventContext(networkConf.name, this)

	private var destroy = false
	private var executor: ExecutorService = _

	setVerbose(globalConf.verbose)
	setName(networkConf.nick)
	setLogin(getName)
	setFinger(networkConf.finger)
	setVersion(getFinger)
	setEncoding(DEFAULT_CHARSET_NAME)

	if (networkConf.servers.isEmpty)
		logWarn("No servers for network " + networkConf.name)
	else
		networkReconnect

	override def onMessage(channel: String, sender: String, login: String, hostname: String, rawMessage: String) {
		val message = decodeMessage(rawMessage)

		parseCommand(message) match {
			case Some((command, args)) => {
				val event = IrcCommandEvent(eventContext, channel, sender, login, hostname, command, args)
				handleEventAsync(event, handlerLists.command.list)
			}
			case None => {
				val event = IrcMessageEvent(eventContext, channel, sender, login, hostname, message)
				handleEventAsync(event, handlerLists.message.list)
			}
		}
	}

	override def onAction(sender: String, login: String, hostname: String, target: String, rawAction: String) {
		val action = decodeMessage(rawAction)

		val event = IrcActionEvent(eventContext, sender, login, hostname, target, action)
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
				handleEvent(event, handlers)
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
		logDebug("Decoding message \"" + Util.escapeNonPrintable(msg) + "\"")


		val bbuf = DEFAULT_CHARSET.newEncoder.encode(CharBuffer.wrap(msg))
		val detector = new CharsetDetector
		detector.setText(new ByteBufferInputStream(bbuf.asReadOnlyBuffer))
		val csName = detector.detect.getName
		logDebug(s"Detected charset: $csName")

		val charset = Charset.forName(csName)
		if (charset == DEFAULT_CHARSET) {
			logDebug("Not decoding")
			msg
		} else {
			val newMsg = Charset.forName(csName).decode(bbuf).toString
			logDebug("Decoded message \"" + newMsg + "\"")
			newMsg
		}
	}
}
