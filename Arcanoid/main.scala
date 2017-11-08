import scala.io.Source


object Process extends App {

	class Vect2D(new_x: Int, new_y: Int) {

		private var _x: Int = new_x
		private var _y: Int = new_y

		def x: Int = _x

		def y: Int = _y

		def x_=(x: Int): Unit = {

			_x = x
		}

		def y_=(y: Int): Unit = {

			_y = y
		}

		def show: Unit = {

			println("(" + x + ", " + y + ")")
		}
	}


	class Ball(new_pos: Vect2D, new_dest: Vect2D) {

		private var _pos: Vect2D = new_pos
		private var _dest: Vect2D = new_dest

		def pos: Vect2D = _pos

		def pos_=(pos: Vect2D): Unit = {

			_pos = pos
		}

		def dest: Vect2D = _dest

		def dest_=(dest: Vect2D): Unit = {

			_dest = dest
		}

		def showPos: Unit = {

			pos.show
		}

		def showDest: Unit = {

			dest.show
		}
	}

/*
	class GameZone() {
	}
*/

	def readZone(file: String): 

	/**
	 *	For now, we will deal only with rectangular areas.
	 */

	def printField(width: Int, height: Int): Unit = {

		var i = 0
		var j = 0

		for (j <- 1 to width)
			print("*")

		println("")

		for (i <- 1 to (height-2)) {

			print("*")

			for (j <- 1 to (width-2)) {
				print(" ")
			}

			print("*")
			println("")
		}

		for (j <- 1 to width)
			print("*")

		println("")
	}


/**
 *   ==================================================
 *
 *       _______   _______   _____    _______
 *      |  _____| | _____ | | __  \  |  _____|
 *      | |       | |   | | | | \  \ | |___
 *      | |       | |   | | | |  | | |  ___|
 *      | |_____  | |___| | | |_/  / | |____________
 *      |_______| |_______| |_____/  |______________|
 *
 *   ==================================================
 */

	var field = readZone("~/Desktop/map.txt");

	Thread.sleep(1000)
}
