package alisa.modules.log

import java.util.regex.Pattern

import alisa.IrcUser
import alisa.util.{WildcardPattern, WildcardMatcher, RegexMatcher, TextMatcher}

object UserMatcher {

	def apply(patterns: List[String]): UserMatcher = {
		val network :: channel :: nick :: login :: hostname :: _ =
			patterns.padTo(5, "").map(createMatcher)
		new UserMatcher(network, channel, nick, login, hostname)
	}

	private def createMatcher(s: String): Option[TextMatcher] =
		if (s == "" || s == "*") None
		else if (s.startsWith("/")) Some(new RegexMatcher(Pattern.compile(s.substring(1))))
		else Some(new WildcardMatcher(new WildcardPattern(s)))
}

class UserMatcher(network: Option[TextMatcher], channel: Option[TextMatcher],
                  nick: Option[TextMatcher], login: Option[TextMatcher],
                  hostname: Option[TextMatcher]) {

	def apply(net: String, chan: String, user: IrcUser): Boolean =
		network.filter(!_.matches(net)).isEmpty &&
		channel.filter(!_.matches(chan)).isEmpty &&
		nick.filter(!_.matches(user.nick)).isEmpty &&
		login.filter(!_.matches(user.login)).isEmpty &&
		hostname.filter(!_.matches(user.hostname)).isEmpty
}
