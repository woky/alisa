package alisa.modules.log

import org.apache.lucene.store.FSDirectory
import java.io.File
import alisa.ModuleProvider
import java.net.{Socket, InetSocketAddress}
import java.util.{List => JList, Collections}
import scala.collection.JavaConversions._

object LogProvider {

	def DEF_ID_TTL = 900

	def DEF_HTTP_PORT = 8090
}

final class LogProvider extends ModuleProvider {

	import LogProvider._

	def name = "log"

	def create(params: Map[String, AnyRef]) = {
		val idxDirStr = params.getOrElse("indexDir", "index").asInstanceOf[String]
		val idxDir = FSDirectory.open(new File(idxDirStr))
		// TODO use params
		val whitelist = params
				.getOrElse("link_whitelist", Collections.emptyList())
				.asInstanceOf[JList[JList[String]]]
				.map(jl => UserMatcher(jl.toList))
		new LogModule(idxDir, new InetSocketAddress(getLocalPublicAddr, DEF_HTTP_PORT),
			DEF_ID_TTL, whitelist)
	}

	def getLocalPublicAddr = {
		val s = new Socket("example.org", 80)
		try {
			s.getLocalAddress
		} finally {
			s.close
		}
	}
}
