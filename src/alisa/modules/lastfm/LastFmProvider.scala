package alisa.modules.lastfm

import alisa.ModuleProvider

object LasfFmProvider {

	final val PARAM_APIKEY = "apiKey"
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
		new LastFmModule(apiKey)
	}
}
