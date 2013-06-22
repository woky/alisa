package alisa.modules.lastfm

import alisa.ModuleProvider

object LasfFmProvider {

	final val PARAM_APIKEY = "apiKey"
	final val PARAM_NOLFMCACHE = "noLfmCache"
	final val DEF_NOLFMCACHE = false
	final val NAME = "lastfm"
}

final class LastFmProvider extends ModuleProvider {

	import LasfFmProvider._

	def name = NAME

	def create(params: Map[String, AnyRef]) = {
		val apiKey = params.get(PARAM_APIKEY) match {
			case Some(s) => s.toString
			case _ => throw new Exception(s"No $PARAM_APIKEY defined")
		}
		val noCache = params.get(PARAM_NOLFMCACHE)
				.map(_.asInstanceOf[Boolean])
				.getOrElse(DEF_NOLFMCACHE)
		new LastFmModule(apiKey, noCache)
	}
}
