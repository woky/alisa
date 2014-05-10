package alisa.util

import java.time.{Period, LocalDateTime, Duration}
import java.time.format.TextStyle
import java.util.Locale

object DateTime {

	def formatPastDateTime(past: LocalDateTime, now: LocalDateTime): String = {
		require(now.isAfter(past))
		def ago(n: Int, u: String) = n.toString + " " + u + (if (n > 1) "s " else " ") + "ago"
		val period = Period.between(past.toLocalDate, now.toLocalDate)
		def monthStr = past.getMonth.getDisplayName(TextStyle.SHORT, Locale.getDefault)
		if (period.getYears > 0 || (past.getYear != now.getYear &&
				(period.getMonths > 0 || period.getDays >= 7))) {
			monthStr + " " + past.getDayOfMonth + " " + past.getYear
		} else if (period.getMonths > 0 || period.getDays >= 7) {
			monthStr + " " + past.getDayOfMonth
		} else if (period.getDays > 1) {
			ago(period.getDays, "day")
		} else {
			val duration = Duration.between(past, now)
			if (duration.toDays == 1) {
				"1 day ago"
			} else if (duration.getSeconds >= 60) {
				duration.toHours.toInt match {
					case hours if hours > 0 => ago(hours, "hour")
					case _ => ago(duration.toMinutes.toInt % 60, "min")
				}
			} else {
				"now"
			}
		}
	}

	def formatDuration(totalSecs: Long, zero: => String = "0s"): CharSequence = {
		val secs = totalSecs % 60
		val totalMins = totalSecs / 60
		val mins = totalMins % 60
		val totalHours = totalMins / 60
		val hours = totalHours % 24
		val totalDays = totalHours / 24

		if (totalDays > 0) {
			if (hours > 0)
				totalDays + "d " + hours + "m"
			else
				totalDays + "d"
		} else if (totalHours > 0) {
			if (mins > 0)
				totalHours + "h " + mins + "m"
			else
				totalHours + "h"
		} else if (totalSecs > 0) {
			if (mins > 0 && secs > 0)
				mins + "m " + secs + "s"
			else if (mins > 0)
				mins + "m"
			else // secs > 0
				secs + "s"
		} else {
			zero
		}
	}
}
