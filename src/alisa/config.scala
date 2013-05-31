package alisa


case class ServerConfig(host: String,
                        port: Int = 6667,
                        reconnTries: Int = 3,
                        reconnDelay: Int = 15000)

case class NetworkConfig(name: String,
                         nick: String,
                         finger: String,
                         servers: Seq[ServerConfig],
                         channels: Seq[ChannelConfig])

case class ChannelConfig(name: String)

case class ModuleConfig(name: String,
                        params: Map[String, AnyRef])

case class AlisaConfig(verbose: Boolean,
                       networks: List[NetworkConfig],
                       modules: List[ModuleConfig])
