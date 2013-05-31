package alisa.modules.log

import com.google.common.cache.CacheBuilder
import java.util.concurrent.TimeUnit

final class AllowedIds(val idTtl: Int) extends ((String) => Option[LuceneChannel]) {

	private val cache = CacheBuilder
			.newBuilder
			.expireAfterWrite(idTtl, TimeUnit.SECONDS)
			.build[String, LuceneChannel]

	def apply(id: String) =
		cache.getIfPresent(id) match {
			case null => None
			case c => Some(c)
		}

	def update(id: String, allowedChan: LuceneChannel) {
		cache.put(id, allowedChan)
	}
}
