package alisa.util

import java.util.regex.Pattern

object WildcardPattern {

	// Credits go to isync (matches() in main.c).
	def matches(text: String, pattern: String): Boolean = {
		def iter(ti: Int, pi: Int): Boolean = {
			if (pi == pattern.length)
				ti == text.length
			else if (pattern.charAt(pi) == '*')
				iter(ti, pi + 1) || (ti != text.length && iter(ti + 1, pi))
			else if (ti != text.length && text.charAt(ti) == pattern.charAt(pi))
				iter(ti + 1, pi + 1)
			else
				false
		}
		iter(0, 0)
	}
}

class WildcardPattern(pattern: String) {

	def apply(text: String): Boolean = WildcardPattern.matches(text, pattern)
}

trait TextMatcher {
	def matches(text: String): Boolean
}
class WildcardMatcher(pattern: WildcardPattern) extends TextMatcher {
	override def matches(text: String) = pattern(text)
}
class RegexMatcher(pattern: Pattern) extends TextMatcher {
	override def matches(text: String) = pattern.matcher(text).matches()
}
