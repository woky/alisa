package alisa.modules.urlinfo

import java.net.{HttpURLConnection, URL}
import java.util.regex.Pattern
import javax.json._
import java.io.{BufferedInputStream, IOException}
import alisa.util.Logger
import javax.json.stream.JsonParsingException
import alisa.util.{MircColors => MC, MessageBuffer}
import alisa.util.Misc._
import alisa.util.DateTime._
import java.time.{LocalDateTime, ZonedDateTime, Duration}
import java.time.format.DateTimeParseException
import java.nio.CharBuffer
import resource._

object YouTube extends UrlHandler with Logger {

	final val STD_HOST_REGEX = Pattern.compile("(?:(?:www|m)\\.)?youtube\\.com")
	final val STD_QUERY_REGEX = Pattern.compile("(?:.*&)?v=([-\\w]+).*")
	final val API_ROOT = "https://www.googleapis.com/youtube/v3/"
	final val QUERY_TPL = (API_ROOT + "videos?part=snippet,contentDetails,statistics&fields=items("
			+ "snippet(publishedAt,title,channelTitle),"
			+ "contentDetails(duration,contentRating/*),"
			+ "statistics(viewCount,likeCount,dislikeCount))")

	override def fill(buf: CharBuffer, config: Config, url: URL): Boolean = {
		def _fill(id: String, ytApiKey: String) =
			getVideoInfo(buf, id, ytApiKey) match {
				case Some(videoInfo) =>
					try {
						addVideoInfo(buf, videoInfo)
						true
					} catch {
						case e@(_: ClassCastException | _: NullPointerException |
								_: DateTimeParseException) =>
							logError(s"Got illegal result [video $id]", e)
							false
					}
				case _ => false
			}

		config.optYtApiKey match {
			case Some(ytApiKey) =>
				if ("youtu.be" == url.getHost) {
					_fill(url.getPath.substring(1), ytApiKey)
				} else if (STD_HOST_REGEX.matcher(url.getHost).matches() &&
						"/watch" == url.getPath) {
					val queryMatch = STD_QUERY_REGEX.matcher(url.getQuery)
					if (queryMatch.matches())
						_fill(queryMatch.group(1), ytApiKey)
					else
						false
				} else {
					false
				}
			case _ => false
		}
	}

	private def getVideoInfo(buf: MessageBuffer, id: String, ytApiKey: String): Option[JsonObject] = {
		val query = QUERY_TPL + "&id=" + id + "&key=" + ytApiKey
		logDebug("GET " + QUERY_TPL)
		try {
			val httpConn = new URL(query).openConnection().asInstanceOf[HttpURLConnection]
			managed(new BufferedInputStream(httpConn.getInputStream)) acquireAndGet { input =>
				httpConn.getResponseCode match {
					case 200 | 203 =>
						val json = Json.createReader(input).read().asInstanceOf[JsonObject]
						try {
							val items = json.getJsonArray("items")
							if (items.size() > 0)
								Some(items.getJsonObject(0))
							else
								None
						} catch {
							case e@(_: ClassCastException | _: NullPointerException) =>
								logError(s"Got illegal result [video $id]", e)
								None
						}
					case code =>
						logError(s"Response has status $code [video $id]")
						None
				}
			}
		} catch {
			case e@(_: IOException | _: JsonException | _: JsonParsingException) =>
				logError(s"Request failed [video $id]", e)
				None
		}
	}

	private def addVideoInfo(buf: MessageBuffer, videoMap: JsonObject) {
		val snippet = videoMap.getJsonObject("snippet")
		val title = snippet.getString("title")
		val publishedAtStr = snippet.getString("publishedAt")
		val contentDetails = videoMap.getJsonObject("contentDetails")
		val durationStr = contentDetails.getString("duration")
		val nsfw = contentDetails.getJsonObject("contentRating") match {
			case null => false
			case contentRating =>
				val mpaa = contentRating.getString("mpaaRating", null)
				val eirin = contentRating.getString("eirinRating", null)
				val yt = contentRating.getString("ytRating", null)
				"mpaaNc17" == mpaa || "eirinR18plus" == eirin || "yt17Plus" == yt ||
						"ytAgeRestricted" == yt
		}
		val stats = videoMap.getJsonObject("statistics")
		val views = stats.getString("viewCount").toInt
		val likes = stats.getString("likeCount").toInt
		val dislikes = stats.getString("dislikeCount").toInt
		val channel = snippet.getString("channelTitle")

		val duration = Duration.parse(durationStr)
		val publishedAt = ZonedDateTime.parse(publishedAtStr).toLocalDateTime

		buf ++= "YT"
		if (nsfw)
			buf += ' ' ++= MC(MC.RED) ++= "NSFW" ++= MC.CLEAR
		buf ++= ": " ++= title
		buf ++= " | " ++= formatDuration(duration.getSeconds, zero = "live")
		buf ++= " | " ++= formatPastDateTime(publishedAt, LocalDateTime.now())
		if (likes > 0 || dislikes > 0) {
			buf ++= " |"
			if (likes > 0)
				buf ++= " ↑" ++= prefixUnit(likes)
			if (dislikes > 0)
				buf ++= " ↓" ++= prefixUnit(dislikes)
		}
		buf ++= " | " ++= prefixUnit(views) ++= " views" /* ++= "👀"*/
		buf ++= " | by " ++= channel
	}
}
