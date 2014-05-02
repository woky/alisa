package alisa.util

import java.util.regex.Pattern
import java.awt.event.KeyEvent
import java.nio.CharBuffer
import java.nio.charset.{IllegalCharsetNameException, Charset}
import com.ibm.icu.text.CharsetDetector
import java.io.UnsupportedEncodingException

object Misc {

	def prefixUnit(value: Long, unit: String = "", unitSep: String = "",
				   si: Boolean = true): String = {
		val prefixSize = if (si) 1000 else 1024
		if (value < prefixSize) {
			value + unitSep + unit
		} else {
			val exp = (Math.log(value) / Math.log(prefixSize)).asInstanceOf[Int]
			val siPrefix = "kMGTPE".charAt(exp - 1)
			val prefix = if (si) siPrefix else siPrefix.toUpper + "i"
			val prefixedVal = value / Math.pow(prefixSize, exp)
			val intVal = prefixedVal.toInt
			val fraction = ((prefixedVal - intVal) * 10).toInt
			"" + intVal + (if (fraction > 0) "." + fraction else "") + unitSep + prefix + unit
		}
	}

	final val DEFAULT_SPLIT_REGEX = Pattern.compile("\\s*[,; ]\\s*")
	final val WS_SPLIT_REGEX = Pattern.compile("\\s+")
	final val URL_SPLIT_REGEX = Pattern.compile("&")
	final val COLON_SPLIT_REGEX = Pattern.compile("\\s*[,;]\\s*")

	def parseArgs(line: String, default: Option[String] = None, limit: Int = -1,
	           regex: Pattern = DEFAULT_SPLIT_REGEX): List[String] =
		regex.split(line.trim, limit).toList match {
			case "" :: Nil =>
				default match {
					case Some(s) => List(s)
					case None => Nil
				}
			case xs => xs
		}

	final val PARAM_REGEX = Pattern.compile("(\\w+)(?:=(\\S+))?")

	/**
	 * Parses line in format
	 * {{{
	 * key1=value1 key2  key3=
	 * }}}
	 * into
	 * {{{
	 * Map(
	 * 	"key1" -> Some("value1")
	 * 	"key2" -> None
	 * 	"key3" -> Some("")
	 * )
	 * }}}
	 *
	 * @param splitRegex `Pattern` on which to split the line
	 * @return
	 */
	def parseMap(line: String, splitRegex: Pattern = WS_SPLIT_REGEX):
			Map[String, Option[String]] = {
		def iter(args: List[String], params: Map[String, Option[String]]): Map[String, Option[String]] =
			args match {
				case arg :: xs =>
					val matcher = PARAM_REGEX.matcher(arg)
					if (matcher.matches)
						matcher.group(2) match {
							case s: String => iter(xs, params + (matcher.group(1) -> Some(s)))
							case _ => iter(xs, params + (matcher.group(1) -> None)) // null
						}
					else
						iter(xs, params)
				case Nil => params
			}

		iter(parseArgs(line.trim, None, regex = splitRegex), Map())
	}

	def parseUrlMap(line: String) = parseMap(line, splitRegex = URL_SPLIT_REGEX)

	def escapeChar(c: Char, p: (Char) => Boolean) =
		if (p(c))
			c.toString
		else
			"\\u%04X".format(c.toShort)

	def isPrintableUnicode(c: Char) =
		if (Character.isISOControl(c) || c == KeyEvent.CHAR_UNDEFINED) {
			false
		} else {
			val block = Character.UnicodeBlock.of(c)
			block != null && block != Character.UnicodeBlock.SPECIALS
		}

	def escapeCharUnicode(c: Char) = escapeChar(c, isPrintableUnicode)

	def isPrintableASCII(c: Char) = c < 0x80 && !Character.isISOControl(c)

	def escapeCharASCII(c: Char) = escapeChar(c, isPrintableASCII)


	def escapeString(s: String, ec: (Char) => String): CharSequence = {
		val sb = new StringBuilder(s.length * 2)
		for (c <- s)
			sb.append(ec(c))
		sb
	}

	def escapeStringUnicode(s: String): CharSequence = escapeString(s, escapeCharUnicode)

	def escapeStringASCII(s: String): CharSequence = escapeString(s, escapeCharASCII)

	lazy val INT_RE = Pattern.compile("^-?\\d+$")

	def looksLikeInt(str: String) = INT_RE.matcher(str).matches

	def parseInt(str: String): Option[Int] =
		try {
			Some(str.toInt)
		} catch {
			case _: NumberFormatException => None
		}

	def recode(buf: CharBuffer, from: Charset, to: Charset): CharBuffer =
		to.decode(from.encode(buf))

	def tryDetectRecode(buf: CharBuffer, from: Charset): CharBuffer = {
		val bbuf = from.newEncoder.encode(buf)
		val detector = new CharsetDetector
		detector.setText(new ByteBufferInputStream(bbuf.asReadOnlyBuffer))
		detector.detect match {
			case null => buf
			case csMatch =>
				try {
					Charset.forName(csMatch.getName) match {
						case `from` => buf
						case charset => charset.decode(bbuf)
					}
				} catch {
					case e@(_: UnsupportedEncodingException |
					        _: IllegalCharsetNameException) => buf
				}
		}
	}
}
