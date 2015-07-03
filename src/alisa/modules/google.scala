package alisa.modules

import java.net.URLEncoder

import alisa.util.Logger
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

final class GoogleSearchModule(apiKey: String, cx: String) extends Module with Logger with CmdHandler {

	override def handles(cmd: String) = cmd == "g" || cmd == "google"
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

	private def handleSearch(ev: IrcCommandEvent, q: String): Unit = {
		val searchUrl = "https://encrypted.google.com/search?q=" + URLEncoder.encode(q, "utf-8")
		ev.bot.sendMessage(ev.channel, searchUrl)
		Try(search(q)) match {
			case Success(Some(results)) =>
				for (((title, link), idx) <- results.zipWithIndex) {
					ev.bot.sendMessage(ev.channel, s"${idx + 1}. $title")
					ev.bot.sendMessage(ev.channel, s"   $link")
				}
			case Success(_) => ev.bot.sendMessage(ev.channel, "No results")
			case Failure(ex) =>
				val reason = ex match {
					case ex: GoogleJsonResponseException => ex.getStatusMessage +
							Option(ex.getDetails).map(" " + _.getMessage).getOrElse("")
					case _ => ex.toString
				}
				logEx(ex)
				ev.bot.sendMessage(ev.channel, s"ERROR: $reason")
		}
	}

	override def handleCommand(ev: IrcCommandEvent): Unit = {
		Option(ev.args.text).filter(!_.isEmpty) match {
			case Some(q) => handleSearch(ev, q)
			case _ => ev.bot.sendMessage(ev.channel, s"${ev.user.nick}, Usage: g QUERY")
		}
	}
}
