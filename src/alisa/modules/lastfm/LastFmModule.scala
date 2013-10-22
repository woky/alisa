/*
 * TODO
 * find out why last.fm API sometimes sends last played track twice and
 * eliminate duplicate code
 */

package alisa.modules.lastfm

import alisa.{SimpleCommandHandler2, Module, IrcCommandEvent}
import alisa.util.Misc._
import java.util.concurrent.ConcurrentHashMap
import de.umass.lastfm._
import alisa.util.{MircColors => MC, Logger}
import scala.collection.JavaConversions._
import resource._
import java.io._
import java.nio.file.{NoSuchFileException, Paths, Files}
import java.util.Collection

private object LastFmModule {

	final val USER_MAP_FILE = "lastfm-usermap"
}

final class LastFmModule(apiKey: String, noLfmCache: Boolean)
		extends Module with SimpleCommandHandler2 with Logger {

	if (noLfmCache)
		Caller.getInstance.setCache(null)

	private type ChanKey = (String, String)

	private def chanKey(event: IrcCommandEvent) = (event.network.name, event.user.user.nick)

	private type UserMap = ConcurrentHashMap[ChanKey, String]
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
								userMap.put(chanKey(event), user)
								saveUserMap
							case Nil =>
								userMap.remove(event.user.user.nick)
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

		val lfmUser = userMap.getOrElse(chanKey(event), event.user.user.nick)
		val results = getTrack(lfmUser, idx)
		if (results.isEmpty)
			return

		val iter = results.iterator
		val npOrLp = iter.next
		val lp =
			if (iter.hasNext)
				iter.next
			else
				npOrLp

		sendLastPlayed(event, lp, lfmUser)
	}

	private def sendLastPlayed(event: IrcCommandEvent, track: Track, lfmUser: String) {
		val msg = new StringBuilder(64)
		appendUser(msg, event.user.user.nick, lfmUser)
		msg += ' ' += MC.BOLD ++= MC(MC.LIGHT_BLUE)
		msg ++= "lp"
		msg += MC.CLEAR += ' '
		appendTrack(msg, track)
		event.network.bot.sendAction(event.channel, msg.toString)
	}

	private def sendNowPlaying(event: IrcCommandEvent) {
		val lfmUser = userMap.getOrElse(chanKey(event), event.user.user.nick)
		val results = getTrack(lfmUser, 1)
		if (results.isEmpty)
			return

		val iter = results.iterator
		val track = iter.next
		if (!iter.hasNext) {
			sendLastPlayed(event, track, lfmUser)
			return
		}

		val msg = new StringBuilder(64)
		appendUser(msg, event.user.user.nick, lfmUser)
		msg += ' ' += MC.BOLD ++= MC(MC.RED)
		msg ++= "np"
		msg += MC.CLEAR += ' '
		appendTrack(msg, results.iterator.next)
		event.network.bot.sendAction(event.channel, msg.toString)
	}

	private def appendUser(sb: StringBuilder, ircUser: String, lfmUser: String) {
		sb ++= ircUser
		if (!ircUser.equals(lfmUser))
			sb ++= " (" ++= lfmUser += ')'
	}

	private def appendTrack(sb: StringBuilder, track: Track) {
		sb ++= MC(MC.LIGHT_GREEN) ++= track.getName += MC.CLEAR
		if (track.getArtist != null && !track.getArtist.isEmpty)
			sb ++= " by " ++= MC(MC.PINK) ++= track.getArtist += MC.CLEAR
		if (track.getAlbum != null && !track.getAlbum.isEmpty)
			sb ++= " on " ++= MC(MC.LIGHT_CYAN) ++= track.getAlbum += MC.CLEAR

		def appendTags(tags: Collection[String]) {
			val it = tags.iterator
			sb ++= " ("
			do {
				sb ++= it.next
				if (it.hasNext)
					sb.append(", ")
			} while (it.hasNext)
			sb += ')'
		}

		if (!track.getTags.isEmpty) {
			appendTags(track.getTags)
		} else {
			val album = Album.getInfo(null, track.getAlbumMbid, apiKey)
			if (album != null && !album.getTags.isEmpty) {
				appendTags(album.getTags)
			} else {
				val artist = Artist.getInfo(track.getArtistMbid, apiKey)
				if (artist != null && !artist.getTags.isEmpty)
					appendTags(artist.getTags)
			}
		}
	}

	private def getTrack(lfmUser: String, pos: Int) =
		User.getRecentTracks(lfmUser, pos, 1, apiKey)

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
