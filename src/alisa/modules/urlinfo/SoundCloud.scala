package alisa.modules.urlinfo

import java.net.{MalformedURLException, HttpURLConnection, URL}
import java.io.IOException
import javax.json.{JsonException, Json, JsonObject}
import javax.json.stream.JsonParsingException
import alisa.util.{Logger, MessageBuffer}
import alisa.util.Misc._
import alisa.util.DateTime._
import org.threeten.bp.Month
import org.threeten.bp.format.TextStyle
import java.util.Locale

object SoundCloud extends UrlHandler with Logger {

	final val API_ROOT = "https://api.soundcloud.com/"

	override def fill(buf: MessageBuffer, config: Config, url: URL): Boolean = {
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
			try {
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
			} finally {
				httpConn.disconnect()
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
			try {
				val input = httpConn.getInputStream
				try {
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
				} finally {
					input.close()
				}
			} finally {
				httpConn.disconnect()
			}
		} catch {
			case e: IOException =>
				logError(s"Request failed [URL $url]", e)
				None
		}
	}

	@throws[ClassCastException]
	@throws[NullPointerException]
	private def addTrackOrPlaylistInfo(buf: MessageBuffer, typ: String, doc: JsonObject) {
		val title = doc.getString("title")
		val user = doc.getJsonObject("user").getString("username")
		val durationMsec = doc.getInt("duration")
		val releaseMonth = doc.getInt("release_month", -1)
		val releaseYear = doc.getInt("release_year", -1)
		val genre = doc.getString("genre")

		buf ++= "SC " ++= typ ++= ": " ++= title
		buf ++= " by " ++= user
		if (releaseYear > 0) {
			buf ++= " in "
			if (releaseMonth > 0) {
				val monthStr = Month.of(releaseMonth).getDisplayName(TextStyle.SHORT,
					Locale.getDefault)
				buf ++= monthStr += ' '
			}
			buf ++= releaseYear
		}
		buf ++= " | " ++= formatDuration(durationMsec / 1000, zero = "live?")
		buf ++= " | " ++= genre
	}

	private def addTrackInfo(buf: MessageBuffer, doc: JsonObject) {
		addTrackOrPlaylistInfo(buf, "track", doc)
	}

	private def addPlaylistInfo(buf: MessageBuffer, doc: JsonObject) {
		addTrackOrPlaylistInfo(buf, "set", doc)
	}

	@throws[ClassCastException]
	@throws[NullPointerException]
	private def addUserInfo(buf: MessageBuffer, doc: JsonObject) {
		val user = doc.getString("username")
		val name = doc.getString("full_name")
		val city = doc.getString("city")
		val country = doc.getString("country")
		val website = doc.getString("website")
		val trackCount = doc.getInt("track_count")
		val followerCount = doc.getInt("followers_count")

		buf ++= "SC user: " ++= name ++= " (" ++= user += ')'
		buf ++= " from " ++= city ++= ", " ++= country
		buf ++= " | " ++= prefixUnit(trackCount) ++= " ♫"
		buf ++= " | " ++= prefixUnit(followerCount) ++= " ★"
		buf ++= " | " ++= website
	}
}
