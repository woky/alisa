package alisa.modules.urlinfo

import java.net.{MalformedURLException, HttpURLConnection, URL}
import java.io.{BufferedInputStream, IOException}
import javax.json.{JsonException, Json, JsonObject}
import javax.json.stream.JsonParsingException
import alisa.util.{Logger, MessageBuffer}
import alisa.util.Misc._
import alisa.util.DateTime._
import java.time.Month
import java.time.format.TextStyle
import java.util.Locale
import java.nio.CharBuffer
import resource._
import scala.Some

object SoundCloud extends UrlHandler with Logger {

	final val API_ROOT = "https://api.soundcloud.com/"

	override def fill(buf: CharBuffer, config: Config, url: URL): Boolean = {
		config.optScClientId match {
			case Some(clientId) =>
				if ("soundcloud.com" == url.getHost) {
					getApiUrl(url, clientId) match {
						case Some(apiUrl) =>
							def _try(proc: () => Unit): Boolean =
								try {
									proc()
									true
								} catch {
									case e@(_: ClassCastException | _: NullPointerException) =>
										logError(s"Got illegal result [URL $apiUrl]", e)
										false
								}

							val path = apiUrl.getPath
							if (path.startsWith("/tracks/")) {
								getInfo(apiUrl, clientId) match {
									case Some(doc) => _try(() => addTrackInfo(buf, doc))
									case _ => false
								}
							} else if (path.startsWith("/playlists/")) {
								getInfo(apiUrl, clientId) match {
									case Some(doc) => _try(() => addPlaylistInfo(buf, doc))
									case _ => false
								}
							} else if (path.startsWith("/users/")) {
								getInfo(apiUrl, clientId) match {
									case Some(doc) => _try(() => addUserInfo(buf, doc))
									case _ => false
								}
							} else {
								false
							}
						case _ => false
					}
				} else {
					false
				}
			case _ => false
		}
	}

	private def getApiUrl(origUrl: URL, clientId: String): Option[URL] = {
		val urlStr = API_ROOT + "resolve.json?url=" + origUrl + "&client_id=" + clientId
		logDebug("GET " + urlStr)
		try {
			val httpConn = new URL(urlStr).openConnection().asInstanceOf[HttpURLConnection]
			httpConn.setInstanceFollowRedirects(false)
			httpConn.connect
			httpConn.getResponseCode match {
				case 302 =>
					val locStr = httpConn.getHeaderField("Location")
					if (locStr != null) {
						try {
							Some(new URL(locStr))
						} catch {
							case e: MalformedURLException =>
								logError(s"Invalid Location header [$locStr] [URL $origUrl]")
								None
						}
					} else {
						logError(s"No Location header [URL $origUrl]")
						None
					}
				case code =>
					logError(s"Response has status $code [URL $origUrl]")
					None
			}
		} catch {
			case e: IOException =>
				logError(s"Request failed [URL $origUrl]", e)
				None
		}
	}

	private def getInfo(url: URL, clientId: String): Option[JsonObject] = {
		logDebug("GET " + url)
		try {
			val httpConn = url.openConnection().asInstanceOf[HttpURLConnection]
			managed(new BufferedInputStream(httpConn.getInputStream)) acquireAndGet { input =>
				httpConn.getResponseCode match {
					case 200 | 203 =>
						try {
							Some(Json.createReader(input).read().asInstanceOf[JsonObject])
						} catch {
							case e@(_: JsonException | _: JsonParsingException) =>
								logError(s"Couldn't parse result [URL $url]", e)
								None
						}
					case code =>
						logError(s"Response has status $code [URL $url]")
						None
				}
			}
		} catch {
			case e: IOException =>
				logError(s"Request failed [URL $url]", e)
				None
		}
	}

	private def has(s: String) = s != null && !s.isEmpty

	private def addTrackOrPlaylistInfo(buf: MessageBuffer, typ: String, doc: JsonObject) {
		val title = doc.getString("title")
		val user = doc.getJsonObject("user").getString("username")
		val durationMsec = doc.getInt("duration")
		val releaseMonth = doc.getInt("release_month", -1)
		val releaseYear = doc.getInt("release_year", -1)
		val genre = doc.getString("genre", null)

		buf ++= typ ++= ": " ++= title
		buf ++= " | by " ++= user
		buf ++= " | " ++= formatDuration(durationMsec / 1000, zero = "live?")
		if (releaseYear > 0) {
			buf ++= " | "
			if (releaseMonth > 0) {
				val monthStr = Month.of(releaseMonth).getDisplayName(TextStyle.SHORT,
					Locale.getDefault)
				buf ++= monthStr += ' '
			}
			buf ++= releaseYear
		}
		if (has(genre))
			buf ++= " | " ++= genre
	}

	private def addTrackInfo(buf: MessageBuffer, doc: JsonObject) {
		addTrackOrPlaylistInfo(buf, "Song", doc)
	}

	private def addPlaylistInfo(buf: MessageBuffer, doc: JsonObject) {
		addTrackOrPlaylistInfo(buf, "Set", doc)
	}

	private def addUserInfo(buf: MessageBuffer, doc: JsonObject) {
		val user = doc.getString("username")
		val name = doc.getString("full_name", null)
		val city = doc.getString("city", null)
		val country = doc.getString("country", null)
		val website = doc.getString("website", null)
		val trackCount = doc.getInt("track_count")
		val followerCount = doc.getInt("followers_count")

		buf ++= "Artist: "

		if (has(name))
			buf ++= name ++= " (" ++= user += ')'
		else
			buf ++= user

		val hasCity = has(city)
		val hasCountry = has(country)
		if (hasCity || hasCountry) {
			buf ++= " from "
			if (hasCity) {
				buf ++= city
				if (hasCountry)
					buf ++= ", " ++= country
			} else {
				buf ++= country
			}
		}

		if (trackCount > 0)
			buf ++= " | " ++= prefixUnit(trackCount) ++= " ♫"

		if (followerCount > 0)
			buf ++= " | " ++= prefixUnit(followerCount) ++= " ★"

		if (has(website))
			buf ++= " | " ++= website
	}
}
