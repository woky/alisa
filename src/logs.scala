package alisa

import com.google.common.cache.CacheBuilder
import java.util.concurrent.TimeUnit
import javax.inject.{Singleton, Inject}
import com.google.inject.AbstractModule
import com.google.inject.multibindings.Multibinder
import com.sun.jersey.guice.JerseyServletModule
import com.sun.jersey.guice.spi.container.servlet.GuiceContainer
import javax.ws.rs._
import core.{Response, StreamingOutput, MediaType}
import java.io.{OutputStreamWriter, BufferedWriter, OutputStream}
import java.text.SimpleDateFormat
import java.util.Date
import org.jibble.pircbot.PircBot
import util.Random
import java.net.{InetAddress, URLEncoder}
import Util._

final class LogsChannelSearchIds(val idTtl: Long) extends ((String) => Option[LuceneChannel]) {

	private val cache = CacheBuilder.newBuilder
			.expireAfterWrite(idTtl, TimeUnit.SECONDS)
			.build[String, LuceneChannel]

	def apply(id: String) =
		cache.getIfPresent(id) match {
			case c: LuceneChannel => Some(c)
			case _ => None //null
		}

	def update(id: String, allowedChan: LuceneChannel) {
		cache.put(id, allowedChan)
	}
}

object LogsCommon {

	object Web {

		final val PATH = "/logs"
		final val ID_PARAM = "id"
		final val QUERY_PARAM = "query"
		final val QUERY_DEFAULT = "*"
		final val SORTBYTIME_PARAM = "sortByTime"
		final val SORTBYTIME_DEFAULT = "true"
		final val LIMIT_PARAM="limit"
		final val LIMIT_DEFAULT = "100"
	}

	object Command {

		final val COMMAND = "log"

		final val LINK = "link"
		final val FLUSH = "flush"
		final val SEARCH = "search"

		final val ID_LEN = 16

		final val USAGE = s"sender, usage: $COMMAND {$LINK [<query>] | $FLUSH | $SEARCH $SEARCH_USAGE}"

		final val SEARCH_USAGE = "<query> [, {limit=n | rel}...]"

		final val SEARCH_PARAM_LIMIT = "limit"
		final val SEARCH_PARAM_RELEVANCE = "rel"

		final val SEARCH_DEFAULT_LIMIT = 1
		final val SEARCH_DEFAULT_REVELANCE = false

		final val LINK_DEFAULT_LIMIT = 100
	}

	final val DEFAULT_ID_TTL = 15 * 60

	private[this] final val dateFormatTmpl = new SimpleDateFormat("yyyy-MM-dd HH:mm")

	def writeTextResults(results: Seq[LuceneStoredMessage], writeFunc: (String) => Unit) {
		val dateFormat = dateFormatTmpl.clone.asInstanceOf[dateFormatTmpl.type]

		def formatTextResult(msg: LuceneStoredMessage) = {
			val time = dateFormat.format(new Date(msg.stored.time))
			s"$time <${msg.stored.nick}> ${msg.stored.message}"
		}

		for (msg <- results)
			writeFunc(formatTextResult(msg))
	}
}

object Logs {
	def apply(idTtl: Int = LogsCommon.DEFAULT_ID_TTL) = new Logs(LogsContext(new LogsChannelSearchIds(idTtl)))
}

final class Logs(context: LogsContext) extends AbstractModule {

	def configure() {
		bind(classOf[LogsContext]).toInstance(context)
		Multibinder.newSetBinder(binder, classOf[ModuleHandlers]).addBinding.to(classOf[LogsHandlers])

		install(new JerseyServletModule {
			override def configureServlets() {
				bind(classOf[LogsResource])
				serve("/*").`with`(classOf[GuiceContainer])
			}
		})
	}
}

final case class LogsContext(allowedIds: LogsChannelSearchIds)

@Singleton
final class LogsHandlers @Inject()(context: LogsContext, lucene: LuceneService, jettyConfig: JettyConfig)
		extends ModuleHandlers {

	override val message = Some(new IrcEventHandler[IrcMessageEvent] {
		def handle(event: IrcMessageEvent) = {
			event match {
				case IrcMessageEvent(IrcEventContext(network, _), channel, sender, login, hostname, msg) => {
					val t = System.currentTimeMillis
					lucene.addMessage(LuceneMessage(LuceneChannel(network, channel), t, sender, login, hostname, msg))
				}
			}
			true
		}
	})

	override val command = Some(new LogsCommandHandler(context, lucene, jettyConfig))
}

final class LogsCommandHandler(context: LogsContext, lucene: LuceneService, jettyConfig: JettyConfig)
		extends SimpleCommandHandler(LogsCommon.Command.COMMAND) {

	import LogsCommon.Command._

	val linkPrefix = {
		val ipStr = InetAddress.getLocalHost.getHostAddress
		"http://" + ipStr + ':' + jettyConfig.httpPort + LogsCommon.Web.PATH + '/'
	}

	def handleCommand(event: IrcCommandEvent) {
		event match {
			case IrcCommandEvent(IrcEventContext(network, bot), channel, sender, _, _, _, argsStr) => {
				val args = mkArgs(argsStr, Some(sender), 2)

				args match {
					case subcmd :: subargs => subcmd match {
						case LINK => subargs match {
							case query :: Nil => link(network, channel, sender, bot, query, LINK_DEFAULT_LIMIT)
							case Nil => link(network, channel, sender, bot, "*", LINK_DEFAULT_LIMIT)
							case _ => throw new AssertionError // shouldn't happen with split() with limit 2
						}
						case FLUSH => lucene.commit
						case SEARCH => subargs match {
							case params :: Nil => search(network, channel, sender, bot, params)
							case Nil => usage(bot, channel, sender)
							case _ => throw new AssertionError // shouldn't happen with split() with limit 2
						}
						case _ => usage(bot, channel, sender)
					}
					case Nil => usage(bot, channel, sender)
				}
			}
		}
	}

	def usage(bot: PircBot, channel: String, sender: String) {
		val searchUsage = s"$SEARCH <query> [, {limit=n | rel}...]"
		bot.sendMessage(channel, s"$sender, usage: $command {$LINK [<query>] | $FLUSH | $searchUsage}")
	}

	def link(network: String, channel: String, sender: String, bot: PircBot, query: String, limit: Int) {
		val id = Random.alphanumeric.take(ID_LEN).mkString
		context.allowedIds(id) = LuceneChannel(network, channel)

		val encQry = URLEncoder.encode(query, "utf-8")
		val link = (linkPrefix + id
				+ '?' + LogsCommon.Web.QUERY_PARAM + '=' + encQry
				+ '&' + LogsCommon.Web.LIMIT_PARAM + '=' + limit)
		val ttl = context.allowedIds.idTtl
		val heading = s"You can browse logs for channel $channel in network $network for $ttl seconds here:"

		lucene.commit
		bot.sendMessage(sender, heading)
		bot.sendMessage(sender, link)
	}

	def search(network: String, channel: String, sender: String, bot: PircBot, argsStr: String) {
		val args = mkArgs(argsStr, None, 2, COLON_SPLIT_REGEX)

		val (query, paramsMap) = args match {
			case userQry :: paramsStr => {
				val q = if (userQry.trim.isEmpty) "*" else userQry
				val p = paramsStr match {
					case pstr :: Nil => mkParams(pstr)
					case Nil => Map[String, Option[String]]()
					case _ => throw new AssertionError // shouldn't happen with split() with limit 2
				}
				(q, p)
			}
			case Nil => ("*", Map[String, Option[String]]())
		}

		val limit = getSearchLimit(paramsMap, bot, channel, sender)
		val sortByTime = !getSortSearchByRelevance(paramsMap)
		val params = LuceneSearchParams(limit, sortByTime, reverse = true)

		lucene.commit
		val results = lucene.search(query, LuceneChannel(network, channel), params)
		LogsCommon.writeTextResults(results, s => bot.sendMessage(channel, s))
	}

	def getSearchLimit(params: Map[String, Option[String]], bot: PircBot, channel: String, sender: String) =
		params.get(SEARCH_PARAM_LIMIT) match {
			case Some(strOpt) =>
				strOpt match {
					case Some(str) =>
						try {
							Integer.parseInt(str)
						} catch {
							case _: NumberFormatException => {
								bot.sendMessage(channel, s"$sender, limit requires integer argument")
								SEARCH_DEFAULT_LIMIT
							}
						}
					case None => {
						bot.sendMessage(channel, s"$sender, limit requires integer argument")
						SEARCH_DEFAULT_LIMIT
					}
				}
			case None => SEARCH_DEFAULT_LIMIT
		}

	def getSortSearchByRelevance(params: Map[String, Option[String]]) =
		params.get(SEARCH_PARAM_RELEVANCE) match {
			case Some(strOpt) => true
			case None => SEARCH_DEFAULT_REVELANCE
		}

}

@Path(LogsCommon.Web.PATH + "/{" + LogsCommon.Web.ID_PARAM + "}")
final class LogsResource @Inject()(context: LogsContext, lucene: LuceneService) {

	import LogsCommon.Web._

	@GET
	@Produces(Array(MediaType.APPLICATION_JSON + "; charset=UTF-8"))
	def getJson(@PathParam(ID_PARAM)
	            id: String,
	            @QueryParam(QUERY_PARAM) @DefaultValue(QUERY_DEFAULT)
	            queryStr: String,
	            @QueryParam(SORTBYTIME_PARAM) @DefaultValue(SORTBYTIME_DEFAULT)
	            sortByTime: Boolean,
				@QueryParam(LIMIT_PARAM) @DefaultValue(LIMIT_DEFAULT)
				limit: Int) =
		getResults(id, queryStr, sortByTime, limit)

	@GET
	@Produces(Array(MediaType.TEXT_PLAIN + "; charset=UTF-8"))
	def getText(@PathParam(ID_PARAM)
	            id: String,
	            @QueryParam(QUERY_PARAM) @DefaultValue(QUERY_DEFAULT)
	            queryStr: String,
	            @QueryParam(SORTBYTIME_PARAM) @DefaultValue(SORTBYTIME_DEFAULT)
	            sortByTime: Boolean,
				@QueryParam(LIMIT_PARAM) @DefaultValue(LIMIT_DEFAULT)
				limit: Int) = {
	val results = getResults(id, queryStr, sortByTime, limit)
		new StreamingOutput {
			def write(output: OutputStream) {
				val writer = new BufferedWriter(new OutputStreamWriter(output))
				try {
					LogsCommon.writeTextResults(results, s => writer.append(s).append("\n"))
				} finally {
					writer.close
				}
			}
		}
	}

	def getResults(id: String, queryStr: String, sortByTime: Boolean, limit: Int) = {
		context.allowedIds(id) match {
			case Some(allowedChan) => {
				val params = LuceneSearchParams(limit, sortByTime)
				lucene.search(queryStr, allowedChan, params)
			}
			case None => throw new WebApplicationException(Response.Status.NOT_FOUND)
		}
	}
}
