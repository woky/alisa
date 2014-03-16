package alisa.modules.lastfm

import alisa.{SimpleCommandHandler2, Module}
import java.util.concurrent.{Future, Callable, Executors, ConcurrentHashMap}
import alisa.util.{MircColors => MC, Logger}
import alisa.util.Misc._
import java.net.{HttpURLConnection, URL}
import javax.xml.parsers.DocumentBuilderFactory
import java.io._
import alisa.IrcCommandEvent
import alisa.util.Xml._
import resource._
import java.nio.file.{NoSuchFileException, Files, Paths}
import scala.collection.JavaConversions._

private object LastFmModule {

	final val USER_MAP_FILE = "lastfm-usermap"
	final val MAX_TAGS = 5

	private val TAGS_XP = xpc("/lfm/toptags/tag/name")
	private val STATUS_XP = xpc("/lfm/@status")
	private val TRACK_XP = xpc("/lfm/recenttracks/track[1]")
	private val NP_XP = xpc("@nowplaying")
	private val ARTIST_XP = xpc("artist")
	private val NAME_XP = xpc("name")
	private val MBID_XP = xpc("mbid")
	private val ALBUM_XP = xpc("album")
	private val MBID_ATTR_XP = xpc("@mbid")
}

final class LastFmModule(apiKey: String) extends Module with SimpleCommandHandler2 with Logger {

	import LastFmModule._

	private type UserKey = (String, String)
	private def userKey(event: IrcCommandEvent) = (event.network.name, event.user.user.login)
	private type UserMap = ConcurrentHashMap[UserKey, String]
	private val userMap = loadUserMap

	private val apiBaseUrl = "https://ws.audioscrobbler.com/2.0/?api_key=" + apiKey
	private val recentBaseUrl = apiBaseUrl + "&method=user.getRecentTracks&limit=1"
	private val trackTagsBaseUrl = apiBaseUrl + "&method=track.getTopTags&mbid="
	private val albumTagsBaseUrl = apiBaseUrl + "&method=album.getTopTags&mbid="
	private val artistTagsBaseUrl = apiBaseUrl + "&method=artist.getTopTags&mbid="

	private val tagsExecutor = Executors.newFixedThreadPool(3)

	override def stop {
		tagsExecutor.shutdownNow
	}

	def handler = Some(this)

	def handles(cmd: String) = cmd == "lf" || cmd == "lfn" || cmd == "np"

	def handleCommand(event: IrcCommandEvent) {
		parseArgs(event.args.decoded, regex = WS_SPLIT_REGEX) match {
			case "user" :: args =>
				args match {
					case user :: Nil => sendRecent(event, newUser = Some(user))
					case Nil =>
						userMap.remove(userKey(event))
						saveUserMap
					case _ =>
				}
			case userOrOffset :: Nil =>
				parseInt(userOrOffset) match {
					case Some(offset) => sendRecent(event, offset = Math.abs(offset))
					case _ => sendRecent(event, newUser = Some(userOrOffset /* user */))
				}
			case Nil => sendRecent(event)
			case _ =>
		}
	}

	private def sendRecent(event: IrcCommandEvent, offset: Int = 0,
						   newUser: Option[String] = None) {
		val lfmUser =
			newUser match {
				case Some(user) =>
					userMap.put(userKey(event), user)
					saveUserMap
					user
				case _ => userMap.getOrElse(userKey(event), {
					val login = event.user.user.login
					if (login.startsWith("~"))
						login.substring(1)
					else
						login
				})
			}

		val nick = event.user.user.nick

		getRecent(lfmUser, offset) match {
			case t: TrackInfo =>
				val buf = new StringBuilder
				buf ++= nick
				if (!nick.equals(lfmUser))
					buf ++= " (" ++= lfmUser += ')'

				buf += ' ' += MC.BOLD
				if (t.np)
					buf ++= MC(MC.RED) ++= "np"
				else
					buf ++= MC(MC.LIGHT_BLUE) ++= "lp"
				buf += MC.CLEAR += ' '

				buf ++= MC(MC.LIGHT_GREEN) ++= t.name += MC.CLEAR
				t.artist.foreach(buf ++= " by " ++= MC(MC.PINK) ++= _ += MC.CLEAR)
				t.album.foreach(buf ++= " on " ++= MC(MC.LIGHT_CYAN) ++= _ += MC.CLEAR)

				if (!t.tags.isEmpty) {
					val it = t.tags.iterator
					buf ++= " ("
					do {
						buf ++= it.next
						if (it.hasNext)
							buf.append(", ")
					} while (it.hasNext)
					buf += ')'
				}

				event.network.bot.sendAction(event.channel, buf.toString)

			case Failure(msg) =>
				event.network.bot.sendMessage(event.channel,
					s"$nick, cannot proceed, $msg, s-sorry ;_;")
		}
	}

	private def doLfmRequest(strUrl: String): Option[XmlNode] = {
		val url = new URL(strUrl)
		val conn = url.openConnection.asInstanceOf[HttpURLConnection]

		val doc =
			try {
				DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(conn.getInputStream)
			} catch {
				case e: Exception =>
					logWarn("Failed to get or parse XML resource " + url, e)
					return None
			}

		if (!evalXpathTextOpt(STATUS_XP, doc).exists(_ == "ok")) {
			logWarn("Last.fm request was not OK. URL: " + url + ", reply:\n" + dumpXml(doc))
			return None
		}

		logDebug("URL: " + url + ", reply:\n" + dumpXml(doc))
		Some(doc)
	}

	private trait Result

	private case class Failure(msg: String) extends Result

	private case class TrackInfo(name: String, artist: Option[String], album: Option[String],
								 np: Boolean, tags: Vector[String]) extends Result

	private def getRecent(lfmUser: String, offset: Int): Result = {
		val xmlDoc = doLfmRequest(s"$recentBaseUrl&user=$lfmUser&page=${offset + 1}")
				.getOrElse(return Failure("Last.fm request failed"))

		val trackNode = evalXpathNodeOpt(TRACK_XP, xmlDoc)
				.getOrElse(return Failure("there are no recent tracks (append last.fm username once?)"))

		val track = evalXpathTextOpt(NAME_XP, trackNode)
				.filter(!_.isEmpty)
				.getOrElse(return Failure("track name is empty"))

		// tfo ~ tags future option
		val trackTfo =
			evalXpathTextOpt(MBID_XP, trackNode)
					.filter(!_.isEmpty)
					.map(getTagsFuture(trackTagsBaseUrl, _))

		val (artistOpt, artistTfo) =
			evalXpathNodeOpt(ARTIST_XP, trackNode) map {
				node =>
					val name = Option(xmlNodeText(node)).filter(!_.isEmpty)
					val tags = evalXpathTextOpt(MBID_ATTR_XP, node)
							.filter(!_.isEmpty)
							.map(getTagsFuture(artistTagsBaseUrl, _))
					(name, tags)
			} getOrElse ((None, None))

		val (albumOpt, albumTfo) =
			evalXpathNodeOpt(ALBUM_XP, trackNode) map {
				node =>
					val name = Option(xmlNodeText(node)).filter(!_.isEmpty)
					val tags = evalXpathTextOpt(MBID_ATTR_XP, node)
							.filter(!_.isEmpty)
							.map(getTagsFuture(albumTagsBaseUrl, _))
					(name, tags)
			} getOrElse ((None, None))

		val np = evalXpathNodeOpt(NP_XP, trackNode).exists(xmlNodeText(_).toBoolean)

		def selectTags(result: Vector[String], done: Boolean,
					   parts: List[Option[Future[Vector[String]]]]): Vector[String] =
			parts match {
				case Some(part) :: xs =>
					if (!done) {
						try {
							val next = result ++ part.get.take(MAX_TAGS - result.length)
							selectTags(next, next.length < MAX_TAGS, xs)
						} catch {
							case e: Exception =>
								logWarn("Tag fetch thread failed", e)
								selectTags(result, done, xs)
						}
					} else {
						part.cancel(true)
						selectTags(result, done, xs)
					}
				case _ :: xs => selectTags(result, done, xs)
				case _ => result
			}
		val tags = selectTags(Vector.empty, false, List(trackTfo, albumTfo, artistTfo))

		TrackInfo(track, artistOpt, albumOpt, np, tags)
	}

	private def getTagsFuture(baseUrl: String, mbid: String) = {
		tagsExecutor.submit(new Callable[Vector[String]] {
			def call = {
				doLfmRequest(baseUrl + mbid) match {
					case Some(xmlDoc) =>
						evalXpathNodeList(TAGS_XP, xmlDoc)
								.take(MAX_TAGS)
								.map(xmlNodeText)
								.toVector
					case _ => Vector.empty
				}
			}
		})
	}

	private def loadUserMap = {
		try {
			managed(new ObjectInputStream(new BufferedInputStream(
				Files.newInputStream(Paths.get(USER_MAP_FILE))))) acquireAndGet {
				in => in.readObject.asInstanceOf[UserMap]
			}
		} catch {
			case _: NoSuchFileException => new UserMap
		}
	}

	private def saveUserMap {
		synchronized {
			managed(new ObjectOutputStream(new BufferedOutputStream(
				Files.newOutputStream(Paths.get(USER_MAP_FILE))))) acquireAndGet {
				in => in.writeObject(userMap)
			}
		}
	}
}
