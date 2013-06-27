package alisa.modules.log

import com.google.common.cache.CacheBuilder
import java.util.concurrent.TimeUnit
import scala.util.Random

final class AllowedIds(val idTtl: Int, val idLen: Int)
		extends ((String) => Option[LuceneChannel]) {

	private val cache = CacheBuilder
			.newBuilder
			.expireAfterWrite(idTtl, TimeUnit.SECONDS)
			.build[String, LuceneChannel]

	def apply(id: String) =
		cache.getIfPresent(id) match {
			case null => None
			case c => Some(c)
		}

	def add(chan: LuceneChannel): String = {
		def iter: String = {
			val id = Random.alphanumeric.take(idLen).mkString
			if (cache.asMap.putIfAbsent(id, chan) == null)
				id
			else
				iter
		}
		iter
	}
}
