package alisa.util

import java.time.{Period, LocalDateTime, Duration}
import java.time.format.TextStyle
import java.util.Locale

object DateTime {

	def formatPastDateTime(past: LocalDateTime, now: LocalDateTime): CharSequence = {
		require(now.isAfter(past))
		val period = Period.between(past.toLocalDate, now.toLocalDate)
		def monthStr() = past.getMonth.getDisplayName(TextStyle.SHORT, Locale.getDefault)
		if (period.getYears > 0 || (past.getYear != now.getYear &&
				(period.getMonths > 0 || period.getDays >= 7))) {
			"on " + monthStr() + " " + past.getDayOfMonth + " " + past.getYear
		} else if (period.getMonths > 0 || period.getDays >= 7) {
			"on " + monthStr() + " " + past.getDayOfMonth
		} else if (period.getDays > 1) {
			period.getDays + "d ago"
		} else {
			val duration = Duration.between(past, now)
			duration.getUnits
			val days = duration.toDays
			assert(days <= 1)
			if (days == 1) {
				"1d ago"
			} else if (duration.getSeconds >= 120) {
				val hours = duration.toHours
				val minutes = duration.toMinutes % 60
				assert(hours > 0 || minutes >= 2)
				if (hours > 0)
					hours + "h ago"
				else
					minutes + "m ago"
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
