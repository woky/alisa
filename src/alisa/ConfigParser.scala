package alisa

import com.typesafe.config._
import java.util.regex.Pattern
import scala.collection.JavaConversions._
import java.io.File
import alisa.util.Logger

object ConfigParser extends Logger {

	val HOST_PORT_SPLIT = Pattern.compile("[:\\s]+")

	def parseFile(path: String): AlisaConfig = parseFile(new File(path))

	def parseFile(file: File): AlisaConfig = build(ConfigFactory.parseFile(file))

	def build(conf: Config): AlisaConfig = {
		val networks = conf.getList("networks").toList.map(mkNetworkConfig).reverse
		val modules = conf.getList("modules").toList.map(mkModuleConfig).reverse
		val verbose =
			if (conf.hasPath("verbose"))
				conf.getBoolean("verbose")
			else
				false
		AlisaConfig(verbose, networks, modules)
	}

	def mkModuleConfig(confObj: ConfigValue): ModuleConfig = {
		if (confObj.valueType == ConfigValueType.STRING) {
			val name = confObj.unwrapped.asInstanceOf[String].toLowerCase
			val params = Map.empty[String, AnyRef]

			ModuleConfig(name, params)
		} else if (confObj.valueType == ConfigValueType.OBJECT) {
			val conf = confObj.asInstanceOf[ConfigObject].toConfig

			val name = conf.getString("name").toLowerCase
			val params =
				if (conf.hasPath("params"))
					conf.getConfig("params").root.unwrapped.toMap
				else
					Map.empty[String, AnyRef]

			ModuleConfig(name, params)
		} else {
			fail("Invalid module configuration", confObj)
		}
	}

	def mkNetworkConfig(confObj: ConfigValue) = {
		if (confObj.valueType == ConfigValueType.OBJECT) {
			val conf = confObj.asInstanceOf[ConfigObject].toConfig

			val name = conf.getString("name")
			val nick = conf.getString("nick")
			val finger =
				if (conf.hasPath("finger"))
					conf.getString("finger")
				else
					nick
			val servers = conf.getList("servers").toList.map(mkServerConfig).reverse
			val channels = conf.getList("channels").toList.map(mkChannelConfig).reverse

			NetworkConfig(name, nick, finger, servers, channels)
		} else {
			fail("Invalid network configuration", confObj)
		}
	}

	def mkServerConfig(confObj: ConfigValue): ServerConfig = {
		if (confObj.valueType == ConfigValueType.STRING) {
			val spec = confObj.unwrapped.asInstanceOf[String]

			val parts = HOST_PORT_SPLIT.split(spec)
			val host = parts(0)

			if (parts.length == 1) {
				ServerConfig(host)
			} else if (parts.length == 2) {
				val port =
					try {
						Integer.parseInt(parts(1))
					} catch {
						case _: NumberFormatException => {
							fail(s"Not a valid port", confObj)
						}
					}

				ServerConfig(host, port)
			} else {
				fail(s"Invalid server configuration", confObj)
			}
		} else if (confObj.valueType == ConfigValueType.OBJECT) {
			val conf = confObj.asInstanceOf[ConfigObject].toConfig

			// pure FP way seemed too verbose
			var srvConf = ServerConfig(conf.getString("host"))

			if (conf.hasPath("port"))
				srvConf = srvConf.copy(port = conf.getInt("port"))
			if (conf.hasPath("reconnTries"))
				srvConf = srvConf.copy(port = conf.getInt("reconnTries"))
			if (conf.hasPath("reconnDelay"))
				srvConf = srvConf.copy(port = conf.getInt("reconnDelay"))

			srvConf
		} else {
			fail("Invalid module configuration", confObj)
		}
	}

	def mkChannelConfig(confObj: ConfigValue): ChannelConfig = {
		def addPrefix(chan: String) = {
			val p = chan(0)
			if (p != '&' && p != '#' && p != '+' && p != '!')
				'#' + chan
			else if (p == '%')
				'#' + chan.substring(1)
			else
				chan
		}

		if (confObj.valueType == ConfigValueType.STRING) {
			val str = confObj.unwrapped.asInstanceOf[String]
			ChannelConfig(addPrefix(str))
		} else {
			fail("Invalid channel configuration", confObj)
		}
	}

	def fail(msg: String, conf: ConfigValue): Nothing = {
		throw new Exception(s"Config error: $msg [${conf.origin}]")
	}
}
