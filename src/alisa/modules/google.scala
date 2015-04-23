package alisa.modules

import java.net.URLEncoder

import alisa.{ModuleProvider, CmdHandler, IrcCommandEvent, Module}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.googleapis.json.GoogleJsonResponseException
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.services.customsearch.Customsearch

import scala.collection.JavaConversions._
import scala.util.{Random, Failure, Success, Try}

final class GoogleSearchProvider extends ModuleProvider {

	def name = "google"

	def create(params: Map[String, AnyRef]) = new GoogleSearchModule(
		params("api_key").asInstanceOf[String], params("cx").asInstanceOf[String])
}

final class GoogleSearchModule(apiKey: String, cx: String) extends Module with CmdHandler {

	override def handles(cmd: String) = cmd == "g"
	override def handler = Some(this)

	private val httpTransport = GoogleNetHttpTransport.newTrustedTransport()
	private val client = new Customsearch.Builder(httpTransport, JacksonFactory.getDefaultInstance,
		null).setApplicationName("alisa").build()

	override def stop: Unit = httpTransport.shutdown()

	private def search(q: String): Option[Iterable[(String, String)]] = {
		val items = client.cse().list(q)
				.setKey(apiKey)
				.setCx(cx)
				.setFields("items(link,title)")
				.setNum(3l)
				.execute().getItems
		Option(items).map(_.map(i => (i.getTitle, i.getLink)))
	}

	override def handleCommand(event: IrcCommandEvent): Unit = {
		val q = Option(event.args.text).filter(!_.isEmpty).getOrElse(Random.nextLong().toString)
		val searchUrl = "https://encrypted.google.com/search?q=" + URLEncoder.encode(q, "utf-8")
		event.bot.sendMessage(event.channel, searchUrl)
		Try(search(q)) match {
			case Success(Some(results)) =>
				for (((title, link), idx) <- results.zipWithIndex) {
					event.bot.sendMessage(event.channel, s"${idx + 1}. $title")
					event.bot.sendMessage(event.channel, s"   $link")
				}
			case Success(_) => event.bot.sendMessage(event.channel, "No results")
			case Failure(ex) =>
				val reason = ex match {
					case ex: GoogleJsonResponseException => ex.getStatusMessage +
							Option(ex.getDetails).map(" " + _.getMessage).getOrElse("")
					case _ => ex.getMessage
				}
				ex.printStackTrace()
				event.bot.sendMessage(event.channel, s"ERROR while googling: $reason")
		}
	}
}
