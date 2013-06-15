package alisa.modules.hooks

import org.junit.runner.RunWith
import alisa._
import alisa.modules.hooks.MsgType._
import HooksHandler._
import alisa.IrcUser
import alisa.IrcMessageEvent
import alisa.IrcNetwork
import org.jibble.pircbot.PircBot
import org.powermock.modules.junit4.PowerMockRunner
import org.powermock.core.classloader.annotations.PrepareForTest

import org.powermock.api.easymock.PowerMock._
import org.junit._
import Assert._
import scala.util.Random
import java.util.regex.Pattern

@RunWith(classOf[PowerMockRunner])
@PrepareForTest(Array(classOf[PircBot]))
class HookHandlerSingleChanTest {

	val TEST_CREATOR = IrcUser("nick1", "login1", "hostname1")
	val NET = "net1"
	val CHAN = "chan1"

	def mkChanHooks(hookList: ChannelHook*): ChannelHooks = Map((NET, CHAN) -> hookList.toList)

	def mkChanHook(patterns: List[EvtPattern],
	               replies: Vector[List[Reply]],
	               name: String = Random.alphanumeric.take(16).mkString,
	               creator: IrcUser = TEST_CREATOR,
	               created: Long = 0,
	               contHooks: Boolean = true,
	               contHandlers: Boolean = true,
	               delay: Option[Long] = None,
	               randDelay: Option[Long] = None): ChannelHook =
		ChannelHook(name, creator, created,
			HookDef(patterns, replies, contHooks, contHandlers, delay, randDelay))

	def mkIrcUser(nick: String,
	              login: String = Random.alphanumeric.take(6).mkString,
	              hostname: String = Random.alphanumeric.take(10).mkString) =
		IrcUser(nick, login, hostname)

	def mkIrcText(s: String) = IrcText(s, s)

	def mkIrcNet(bot: PircBot, name: String = NET) = IrcNetwork(name, bot)

	def mkMsg(bot: PircBot, nick: String, text: String, chan: String = CHAN) =
		IrcMessageEvent(mkIrcNet(bot), chan, mkIrcUser(nick), mkIrcText(text))

	def mkAct(bot: PircBot, nick: String, text: String, chan: String = CHAN) =
		IrcActionEvent(mkIrcNet(bot), mkIrcUser(nick), chan, mkIrcText(text))

	def mkJoin(bot: PircBot, nick: String, chan: String = CHAN) =
		IrcJoinEvent(mkIrcNet(bot), chan, mkIrcUser(nick))

	def mkPart(bot: PircBot, nick: String, chan: String = CHAN) =
		IrcPartEvent(mkIrcNet(bot), chan, mkIrcUser(nick))

	@Test
	def testAnySpeech {
		val chanHooks = mkChanHooks(
			mkChanHook(
				List(EvtPattern(MSG, None, None)),
				Vector(List(Reply(MSG, "Silence!")))
			)
		)

		val bot = createMock(classOf[PircBot])
		bot.sendMessage("chan1", "Silence!")
		bot.sendMessage("chan1", "Silence!")
		bot.sendMessage("chan1", "Silence!")
		bot.sendMessage("chan1", "Silence!")
		bot.sendMessage("chan1", "Silence!")
		replay(bot)

		val handler = new HooksHandler(chanHooks)
		assertTrue(handler.handle(mkJoin(bot, "joe")))
		assertTrue(handler.handle(mkMsg(bot, "joe", "Hi")))
		assertTrue(handler.handle(mkMsg(bot, "juliette", "Hello Joe.")))
		assertTrue(handler.handle(mkPart(bot, "l33tkid")))
		assertTrue(handler.handle(mkMsg(bot, "joe", "Hello Juliette :-)")))
		assertTrue(handler.handle(mkMsg(bot, "juliette", "^^")))
		assertTrue(handler.handle(mkPart(bot, "sergey")))
		assertTrue(handler.handle(mkJoin(bot, "MWolf")))
		assertTrue(handler.handle(mkMsg(bot, "MWolf", "how can i hax plz?")))
		verify(bot)
	}

	@Test
	def testAnySpeechAndHl {
		val chanHooks = mkChanHooks(
			mkChanHook(
				List(EvtPattern(MSG, None, None)),
				Vector(List(Reply(MSG, "%3$s, Silence!")))
			)
		)

		val bot = createMock(classOf[PircBot])
		bot.sendMessage("chan1", "joe, Silence!")
		bot.sendMessage("chan1", "juliette, Silence!")
		bot.sendMessage("chan1", "joe, Silence!")
		bot.sendMessage("chan1", "juliette, Silence!")
		bot.sendMessage("chan1", "MWolf, Silence!")
		replay(bot)

		val handler = new HooksHandler(chanHooks)
		assertTrue(handler.handle(mkJoin(bot, "joe")))
		assertTrue(handler.handle(mkMsg(bot, "joe", "Hi")))
		assertTrue(handler.handle(mkMsg(bot, "juliette", "Hello Joe.")))
		assertTrue(handler.handle(mkPart(bot, "l33tkid")))
		assertTrue(handler.handle(mkMsg(bot, "joe", "Hello Juliette :-)")))
		assertTrue(handler.handle(mkMsg(bot, "juliette", "^^")))
		assertTrue(handler.handle(mkPart(bot, "sergey")))
		assertTrue(handler.handle(mkJoin(bot, "MWolf")))
		assertTrue(handler.handle(mkMsg(bot, "MWolf", "how can i hax plz?")))
		verify(bot)
	}

	@Test
	def testSpeechMultiple {
		val chanHooks = mkChanHooks(
			mkChanHook(
				List(EvtPattern(MSG, None, None)),
				Vector(List(Reply(MSG, "%3$s, Silence!")))
			),
			mkChanHook(
				List(EvtPattern(MSG, None, Some(Pattern.compile("(?i)^(hi|hello)")))),
				Vector(List(Reply(MSG, "%3$s, %8$s")))
			)
		)

		val bot = createMock(classOf[PircBot])
		bot.sendMessage("chan1", "joe, Silence!")
		bot.sendMessage("chan1", "joe, Hi")
		bot.sendMessage("chan1", "juliette, Silence!")
		bot.sendMessage("chan1", "juliette, Hello")
		bot.sendMessage("chan1", "joe, Silence!")
		bot.sendMessage("chan1", "joe, Hello")
		bot.sendMessage("chan1", "juliette, Silence!")
		bot.sendMessage("chan1", "MWolf, Silence!")
		replay(bot)

		val handler = new HooksHandler(chanHooks)
		assertTrue(handler.handle(mkJoin(bot, "joe")))
		assertTrue(handler.handle(mkMsg(bot, "joe", "Hi")))
		assertTrue(handler.handle(mkMsg(bot, "juliette", "Hello Joe.")))
		assertTrue(handler.handle(mkPart(bot, "l33tkid")))
		assertTrue(handler.handle(mkMsg(bot, "joe", "Hello Juliette :-)")))
		assertTrue(handler.handle(mkMsg(bot, "juliette", "^^")))
		assertTrue(handler.handle(mkPart(bot, "sergey")))
		assertTrue(handler.handle(mkJoin(bot, "MWolf")))
		assertTrue(handler.handle(mkMsg(bot, "MWolf", "how can i hax plz?")))
		verify(bot)
	}

	@Test
	def testSpeechMultipleAndStop {
		val chanHooks = mkChanHooks(
			mkChanHook(
				List(EvtPattern(MSG, None, Some(Pattern.compile("(?i)^(hi|hello)")))),
				Vector(List(Reply(MSG, "%3$s, %8$s"))),
				contHooks = false,
				contHandlers = false
			),
			mkChanHook(
				List(EvtPattern(MSG, None, None)),
				Vector(List(Reply(MSG, "%3$s, Silence!")))
			)
		)

		val bot = createMock(classOf[PircBot])
		bot.sendMessage("chan1", "joe, Hi")
		bot.sendMessage("chan1", "juliette, Hello")
		bot.sendMessage("chan1", "joe, Hello")
		bot.sendMessage("chan1", "juliette, Silence!")
		bot.sendMessage("chan1", "MWolf, Silence!")
		replay(bot)

		val handler = new HooksHandler(chanHooks)
		assertTrue(handler.handle(mkJoin(bot, "joe")))
		assertFalse(handler.handle(mkMsg(bot, "joe", "Hi")))
		assertFalse(handler.handle(mkMsg(bot, "juliette", "Hello Joe.")))
		assertTrue(handler.handle(mkPart(bot, "l33tkid")))
		assertFalse(handler.handle(mkMsg(bot, "joe", "Hello Juliette :-)")))
		assertTrue(handler.handle(mkMsg(bot, "juliette", "^^")))
		assertTrue(handler.handle(mkPart(bot, "sergey")))
		assertTrue(handler.handle(mkJoin(bot, "MWolf")))
		assertTrue(handler.handle(mkMsg(bot, "MWolf", "how can i hax plz?")))
		verify(bot)
	}

	@Test
	def testMultiple {
		val chanHooks = mkChanHooks(
			mkChanHook(
				List(
					EvtPattern(
						JOIN,
						Some(UserPattern(Some(Pattern.compile("joe")), None, None)),
						None
					)
				),
				Vector(List(Reply(ACT, "pets %3$s")))
			),
			mkChanHook(
				List(EvtPattern(MSG, None, Some(Pattern.compile("(?i)^(hi|hello)$")))),
				Vector(List(Reply(MSG, "Sup %3$s"))),
				contHooks = false
			),
			mkChanHook(
				List(EvtPattern(MSG, None, None)),
				Vector(List(Reply(MSG, "%3$s, Silence!")))
			),
			mkChanHook(
				List(
					EvtPattern(
						PART,
						Some(UserPattern(Some(Pattern.compile("sergey")), None, None)),
						None
					),
					EvtPattern(
						PART,
						Some(UserPattern(None, None, Some(Pattern.compile("\\.ru$")))),
						None
					)
				),
				Vector(List(Reply(MSG, "dasvidania %3$s")))
			),
			mkChanHook(
				List(EvtPattern(MSG, None, Some(Pattern.compile("(?i)^(hi|hello)")))),
				Vector(List(Reply(MSG, "%3$s, %8$s")))
			)
		)

		val bot = createMock(classOf[PircBot])
		bot.sendAction("chan1", "pets joe")
		bot.sendMessage("chan1", "Sup joe")
		bot.sendMessage("chan1", "juliette, Silence!")
		bot.sendMessage("chan1", "juliette, Hello")
		bot.sendMessage("chan1", "dasvidania l33tkid")
		bot.sendMessage("chan1", "joe, Silence!")
		bot.sendMessage("chan1", "joe, Hello")
		bot.sendMessage("chan1", "juliette, Silence!")
		bot.sendMessage("chan1", "dasvidania sergey")
		bot.sendMessage("chan1", "MWolf, Silence!")
		replay(bot)

		val handler = new HooksHandler(chanHooks)
		assertTrue(handler.handle(mkJoin(bot, "joe")))
		assertTrue(handler.handle(mkMsg(bot, "joe", "Hi")))
		assertTrue(handler.handle(mkMsg(bot, "juliette", "Hello Joe.")))
		assertTrue(handler.handle(IrcPartEvent(mkIrcNet(bot), CHAN,
			IrcUser("l33tkid", "l33tkid", "l33thost.ru"))))
		assertTrue(handler.handle(mkMsg(bot, "joe", "Hello Juliette :-)")))
		assertTrue(handler.handle(mkMsg(bot, "juliette", "^^")))
		assertTrue(handler.handle(mkPart(bot, "sergey")))
		assertTrue(handler.handle(mkJoin(bot, "MWolf")))
		assertTrue(handler.handle(mkMsg(bot, "MWolf", "how can i hax plz?")))
		verify(bot)
	}
}
