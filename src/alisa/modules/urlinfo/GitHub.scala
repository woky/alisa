package alisa.modules.urlinfo

import java.net.URL
import java.nio.CharBuffer
import java.time.ZonedDateTime

import alisa.util.DateTime._
import alisa.util.Logger
import alisa.util.MessageBuffer._

import scala.sys.process._

/*
	TODO
	handle RuntimeException from process API
	handle DateTimeParseException
*/

object GitHub extends UrlHandler with Logger {

	final val API_REPOS = "https://api.github.com/repos/"

	final val JQ_FILTER = """
	([
		"GitHub:", .full_name, "|",
		.description, "|",
		(.forks | tostring), "forks", "|",
		(.stargazers_count | tostring), "★", "|",
		"Active %ACTIVE%"
	] | join(" ")),
	.updated_at
	"""

	final val USER_REPO_REGEX = "^/([^/]+/[^/]+)(?:/.*)?$".r

	override def fill(buf: CharBuffer, config: Config, url: URL): Boolean = {
		if (url.getHost.endsWith("github.com")) {
			url.getPath match {
				case USER_REPO_REGEX(userRepo) =>
					(Seq("curl", "-sS", API_REPOS + userRepo) #| Seq("jq", "-r", JQ_FILTER))
							.lineStream.take(2).toList match {
						case titleTpl :: activeTimeIso :: Nil =>
							ZonedDateTime.parse(activeTimeIso).toLocalDateTime
							val activeTimeDt = ZonedDateTime.parse(activeTimeIso).toLocalDateTime
							val activeTimeHuman = formatPastDateTime(activeTimeDt)
							val title = titleTpl.replace("%ACTIVE%", activeTimeHuman)
							buf ++= title
							true
						case l =>
							logError("jq output:\n" + l.mkString("\n"))
							false
					}
				case _ => false
			}
		}
		else
			false
	}
}
