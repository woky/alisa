package alisa.util

import java.util.regex.Pattern
import java.awt.event.KeyEvent
import java.util.logging.{Logger => JDKLogger}

object Misc {

	def prefixUnit(value: Long, suffix: String, si: Boolean = true): String = {
		val unit = if (si) 1000 else 1024
		if (value < unit) {
			value + " " + suffix
		} else {
			val exp = (Math.log(value) / Math.log(unit)).asInstanceOf[Int]
			val siPrefix = "kMGTPE".charAt(exp - 1)
			val prefix = if (si) siPrefix else (siPrefix.toUpper + "i")
			"%.1f %s%s".format(value / Math.pow(unit, exp), prefix, suffix)
		}
	}

	val DEFAULT_SPLIT_REGEX = Pattern.compile("\\s*[,; ]\\s*")
	val WS_SPLIT_REGEX = Pattern.compile("\\s+")
	val COLON_SPLIT_REGEX = Pattern.compile("\\s*[,;]\\s*")

	def mkArgs(line: String, default: Option[String] = None, limit: Int = -1,
	           regex: Pattern = DEFAULT_SPLIT_REGEX): List[String] =
		regex.split(line.trim, limit).toList match {
			case "" :: Nil =>
				default match {
					case Some(s) => List(s)
					case None => Nil
				}
			case xs => xs
		}

	val PARAM_REGEX = Pattern.compile("(\\w+)(?:=(\\S+))?")

	def mkParams(line: String) = {
		def iter(args: List[String], params: Map[String, Option[String]]): Map[String, Option[String]] =
			args match {
				case arg :: xs => {
					val matcher = PARAM_REGEX.matcher(arg)
					if (matcher.matches)
						matcher.group(2) match {
							case s: String => iter(xs, params + (matcher.group(1) -> Some(s)))
							case _ => iter(xs, params + (matcher.group(1) -> None)) // null
						}
					else
						iter(xs, params)
				}
				case Nil => params
			}

		iter(mkArgs(line.trim, None, regex = WS_SPLIT_REGEX), Map())
	}


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
}
