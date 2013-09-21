package alisa.util

object MircColors {

	val COLOR_CODE	= '\u0003'
	val BOLD		= '\u0002'
	val UNDERLINE	= '\u001F'
	val INVERSE		= '\u0016'
	val CLEAR		= '\u000F'

	val WHITE		= "00"
	val BLACK		= "01"
	val BLUE		= "02"
	val GREEN		= "03"
	val RED			= "04"
	val BROWN		= "05"
	val PURPLE		= "06"
	val ORANGE		= "07"
	val YELLOW		= "08"
	val LIGHT_GREEN	= "09"
	val TEAL		= "10"
	val LIGHT_CYAN	= "11"
	val LIGHT_BLUE	= "12"
	val PINK		= "13"
	val GREY		= "14"
	val LIGHT_GREY	= "15"

	def apply(fg: String): String = COLOR_CODE + fg

	def apply(fg: String, bg: String): String = COLOR_CODE + fg + ',' + bg
}
