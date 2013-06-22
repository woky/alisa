package alisa.modules.lastfm

import alisa.{SimpleCommandHandler2, Module}
import alisa.util.Misc._
import java.util.concurrent.ConcurrentHashMap
import de.umass.lastfm.{Caller, Track, User}
import alisa.util.{MircColors => MC, Logger}
import scala.collection.JavaConversions._
import resource._
import java.io._
import java.nio.file.{NoSuchFileException, Paths, Files}
import scala.Some
import alisa.IrcCommandEvent

private object LastFmModule {

	final val USER_MAP_FILE = "lastfm-usermap"
}

final class LastFmModule(apiKey: String, noLfmCache: Boolean)
		extends Module with SimpleCommandHandler2 with Logger {

	if (noLfmCache)
		Caller.getInstance.setCache(null)

	private type UserMap = ConcurrentHashMap[String, String]
	private val userMap = loadUserMap

	def handler = Some(this)

	def handles(cmd: String) = cmd == "lf" || cmd == "lfn"

	def handleCommand(event: IrcCommandEvent) {
		event.command match {
			case "lf" =>
				mkArgs(event.args.decoded, regex = WS_SPLIT_REGEX) match {
					case "user" :: args =>
						args match {
							case user :: Nil =>
								userMap.put(event.user.nick, user)
								saveUserMap
							case Nil =>
								userMap.remove(event.user.nick)
								saveUserMap
							case _ =>
						}
					case strIdx :: Nil => sendLastPlayed(event, Some(strIdx))
					case Nil => sendLastPlayed(event, None)
					case _ =>
				}
			case "lfn" => sendNowPlaying(event)
		}
	}

	private def sendLastPlayed(event: IrcCommandEvent, optIdx: Option[String]) {
		val idx = optIdx match {
			case Some(s) =>
				try {
					Math.abs(s.toInt)
				} catch {
					case _: NumberFormatException => return
				}
			case _ => 1
		}

		val results = getTrack(event.user.nick, idx)
		if (results.isEmpty)
			return

		val iter = results.iterator
		val npOrLp = iter.next
		val lp =
			if (iter.hasNext)
				iter.next
			else
				npOrLp

		sendLastPlayed(event, lp)
	}

	private def sendLastPlayed(event: IrcCommandEvent, track: Track) {
		val msg = new StringBuilder(64)
		msg ++= event.user.nick += ' ' += MC.BOLD ++= MC(MC.LIGHT_BLUE)
		msg ++= "lp"
		msg += MC.CLEAR += ' '
		appendTrack(msg, track)
		event.network.bot.sendAction(event.channel, msg.toString)
	}

	private def sendNowPlaying(event: IrcCommandEvent) {
		val results = getTrack(event.user.nick, 1)
		if (results.isEmpty)
			return

		val iter = results.iterator
		val track = iter.next
		if (!iter.hasNext) {
			sendLastPlayed(event, track)
			return
		}

		val msg = new StringBuilder(64)
		msg ++= event.user.nick += ' ' += MC.BOLD ++= MC(MC.RED)
		msg ++= "np"
		msg += MC.CLEAR += ' '
		appendTrack(msg, results.iterator.next)
		event.network.bot.sendAction(event.channel, msg.toString)
	}

	private def appendTrack(sb: StringBuilder, track: Track) {
		sb ++= MC(MC.LIGHT_GREEN) ++= track.getName += MC.CLEAR += ' '
		sb ++= "by " ++= MC(MC.PINK) ++= track.getArtist += MC.CLEAR += ' '
		if (track.getAlbum != null && !track.getAlbum.isEmpty) {
			sb ++= "on " ++= MC(MC.LIGHT_CYAN) ++= track.getAlbum += MC.CLEAR
		}
	}

	private def getTrack(nick: String, pos: Int) =
		User.getRecentTracks(userMap.getOrElse(nick, nick), pos, 1, apiKey)

	private def loadUserMap = {
		try {
			managed(new ObjectInputStream(new BufferedInputStream(
				Files.newInputStream(Paths.get(LastFmModule.USER_MAP_FILE))))) acquireAndGet {
				in => in.readObject.asInstanceOf[UserMap]
			}
		} catch {
			case _: NoSuchFileException => new UserMap
		}
	}

	private def saveUserMap {
		synchronized {
			managed(new ObjectOutputStream(new BufferedOutputStream(
				Files.newOutputStream(Paths.get(LastFmModule.USER_MAP_FILE))))) acquireAndGet {
				in => in.writeObject(userMap)
			}
		}
	}
}
