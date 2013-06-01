package alisa.util

object MircColors {

	val COLOR_CODE	= '\u0003'
	val BOLD		= '\u0002'
	val UNDERLINE	= '\u001F'
	val INVERSE		= '\u0016'
	val CLEAR		= '\u000F'

	val WHITE		= 0
	val BLACK		= 1
	val BLUE		= 2
	val GREEN		= 3
	val RED			= 4
	val BROWN		= 5
	val PURPLE		= 6
	val ORANGE		= 7
	val YELLOW		= 8
	val LIGHT_GREEN	= 9
	val TEAL		= 10
	val LIGHT_CYAN	= 11
	val LIGHT_BLUE	= 12
	val PINK		= 13
	val GREY		= 14
	val LIGHT_GREY	= 15

	def apply(fg: Int): String = COLOR_CODE + "%02d".format(fg)

	def apply(fg: Int, bg: Int): String = COLOR_CODE + "%02d,%02d".format(fg, bg)
}
