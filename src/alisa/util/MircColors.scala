package alisa.util

object MircColors {

	final val COLOR_CODE    = '\u0003'
	final val BOLD          = '\u0002'
	final val UNDERLINE     = '\u001F'
	final val INVERSE       = '\u0016'
	final val CLEAR         = '\u000F'

	final val WHITE         = "00"
	final val BLACK         = "01"
	final val BLUE          = "02"
	final val GREEN         = "03"
	final val RED           = "04"
	final val BROWN         = "05"
	final val PURPLE        = "06"
	final val ORANGE        = "07"
	final val YELLOW        = "08"
	final val LIGHT_GREEN   = "09"
	final val TEAL          = "10"
	final val LIGHT_CYAN    = "11"
	final val LIGHT_BLUE    = "12"
	final val PINK          = "13"
	final val GREY          = "14"
	final val LIGHT_GREY    = "15"

	def apply(fg: String): String = COLOR_CODE + fg

	def apply(fg: String, bg: String): String = COLOR_CODE + fg + ',' + bg
}
