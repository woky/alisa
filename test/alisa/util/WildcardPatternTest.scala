package alisa.util

import org.junit.Test
import org.junit.Assert._

class WildcardPatternTest {

	@Test def testNoStar(): Unit = {
		assertTrue(WildcardPattern.matches("Alisa", "Alisa"))
		assertFalse(WildcardPattern.matches("Alisa ", "Alisa"))
		assertFalse(WildcardPattern.matches("Alisa", "Alisa "))
	}

	@Test def testStars(): Unit = {
		assertTrue(WildcardPattern.matches("Miii", "Mi*"))
		assertTrue(WildcardPattern.matches("Mi", "Mi*"))
		assertFalse(WildcardPattern.matches("M", "Mi*"))

		assertTrue(WildcardPattern.matches("Miii-chan", "Mi*chan"))
		assertTrue(WildcardPattern.matches("Michan", "Mi*chan"))
		assertFalse(WildcardPattern.matches("Mi", "Mi*chan"))

		assertTrue(WildcardPattern.matches("Miii-chan", "*chan"))
		assertFalse(WildcardPattern.matches("Miii-chan", "A*chan"))

		assertTrue(WildcardPattern.matches("chan", "*chan*"))
		assertTrue(WildcardPattern.matches("Miii-chan", "*chan*"))
		assertTrue(WildcardPattern.matches("chan !!", "*chan*"))
		assertTrue(WildcardPattern.matches("Miii-chan !!", "*chan*"))

		assertTrue(WildcardPattern.matches("Miii-chan !!", "*ch*n*"))
		assertTrue(WildcardPattern.matches("chn", "*ch*n*"))
		assertFalse(WildcardPattern.matches("cn", "*ch*n*"))
		assertFalse(WildcardPattern.matches("", "*ch*n*"))

		assertTrue(WildcardPattern.matches("", "*"))
		assertTrue(WildcardPattern.matches("M", "*"))
		assertTrue(WildcardPattern.matches("", ""))
		assertFalse(WildcardPattern.matches("M", ""))
	}
}
