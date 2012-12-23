package alisa

import com.google.common.cache.{CacheLoader, CacheBuilder}
import com.ibm.icu.text.CharsetDetector
import java.nio.charset.spi.CharsetProvider
import java.nio.{CharBuffer, ByteBuffer}
import java.nio.charset.{CharsetDecoder, Charset}
import scala.collection.JavaConversions._

object IrcCharsetCommon {

	final val CHARSET_NAME = "pircbot_hack"
	final val OUTPUT_CHARSET = Charset.forName("utf-8")
	final val MAX_CACHED_DECODERS = 16
	final val CHARSETS = List[Charset](IrcCharset)
}

object IrcCharset extends Charset(IrcCharsetCommon.CHARSET_NAME, Array()) {

	def contains(cs: Charset) = false // XXX ?

	def newDecoder = new IrcCharsetDecoder(this)

	def newEncoder = IrcCharsetCommon.OUTPUT_CHARSET.newEncoder
}

final class IrcCharsetDecoder(cs: Charset) extends CharsetDecoder(cs, 1f, 1f) {

	private val decoders = CacheBuilder.newBuilder
			.maximumSize(IrcCharsetCommon.MAX_CACHED_DECODERS)
			.build[String, CharsetDecoder](
		new CacheLoader[String, CharsetDecoder] {
			def load(charsetName: String) =
				Charset.forName(charsetName).newDecoder
						.onMalformedInput(malformedInputAction())
						.onUnmappableCharacter(unmappableCharacterAction())
		})

	// probably not very efficient
	def decodeLoop(in: ByteBuffer, out: CharBuffer) = {
		val detector = new CharsetDetector
		detector.setText(new ByteBufferInputStream(in))
		val csMatch = detector.detect
		decoders.get(csMatch.getName).decode(in, out, true)
	}
}

final class IrcCharsetProvider extends CharsetProvider {

	import IrcCharsetCommon._

	def charsets = CHARSETS.iterator

	def charsetForName(charsetName: String) = if (charsetName == CHARSET_NAME) IrcCharset else null
}