package alisa

import com.google.inject.Module


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

case class GlobalConfig(verbose: Boolean = true,
                        maxMsgSize: Int = 512)

case class Config(global: GlobalConfig = new GlobalConfig,
                  networks: Seq[NetworkConfig],
                  handlers: List[ModuleHandlers] = Nil,
                  modules: List[Module] = Nil)
