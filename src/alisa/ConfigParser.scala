package alisa

import com.typesafe.config._
import ConfigValueType._
import java.util.regex.Pattern
import scala.collection.JavaConverters._
import java.io.File
import alisa.util.Logger

object ConfigParser extends Logger {

	val HOST_PORT_SPLIT = Pattern.compile("[:\\s]+")

	def parseFile(path: String): AlisaConfig = parseFile(new File(path))

	def parseFile(file: File): AlisaConfig = build(ConfigFactory.parseFile(file))

	def build(conf: Config): AlisaConfig = {
		val networks = conf.getList("networks").asScala.map(mkNetworkConfig).toList
		val modules = mkModuleConfigs(conf.getList("modules").asScala.toList, Nil)
		val verbose =
			if (conf.hasPath("verbose"))
				conf.getBoolean("verbose")
			else
				false
		AlisaConfig(verbose, networks, modules)
	}

	def mkModuleConfigs(configObjList: List[ConfigValue], result: List[ModuleConfig])
	: List[ModuleConfig] =
		configObjList match {
			case name :: params :: xs if STRING == name.valueType() &&
					OBJECT == params.valueType() =>
				val paramMap = params.asInstanceOf[ConfigObject].toConfig.root.unwrapped.asScala.toMap
				val modConf = ModuleConfig(name.unwrapped().asInstanceOf[String], paramMap)
				mkModuleConfigs(xs, modConf :: result)
			case name :: xs if STRING == name.valueType() =>
				val modConf = ModuleConfig(name.unwrapped().asInstanceOf[String],
					Map.empty[String, AnyRef])
				mkModuleConfigs(xs, modConf :: result)
			case Nil => result.reverse
			case unknown :: _ => fail(s"Invalid module configuration", unknown)
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
			val servers = conf.getList("servers").asScala.toList.map(mkServerConfig)
			val channels = conf.getList("channels").asScala.toList.map(mkChannelConfig)
			val delay =
				if (conf.hasPath("delay"))
					conf.getInt("delay")
				else
					1000

			NetworkConfig(name, nick, finger, servers, channels, delay)
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
