import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Map

object J_PipeMaze {
	case class CC(row: Int, col: Int)
	
	def solution() = {
		val input = Source.fromFile("src/main/resource/10_prod.txt").getLines.toList
		val emptyLine = List.fill(input.head.length + 2)(".").mkString
		val board = List(emptyLine) ++ input.map(line => "." + line + ".") ++ List(emptyLine)
		
		val start = board.map(line => line.indexOf("S"))
			.zipWithIndex.filter(t => t._1 != -1)
			.map(t => CC(t._2, t._1))
			.head
		
		var q = Queue(start)
		var tiles = Map[CC, Int](start -> 0)
		
		while (q.nonEmpty) {
			val next = q.dequeue()
			val symbol = board(next.row).charAt(next.col)
			val neighborDirs =
				if (symbol != 'S') nMap(symbol)
				else List(CC(-1, 0), CC(0, -1)) // prod
//				else List(CC(1, 0), CC(0, 1)) // test
			neighborDirs.map(dir => CC(next.row + dir.row, next.col + dir.col))
				.filter(cc => !tiles.contains(cc))
				.foreach(cc => {
					q.enqueue(cc)
					tiles(cc) = tiles(next) + 1
				})
		}
		
		val res1 = tiles.values.max
		println(res1)
		
		val res2 = board
			.map(line => line.replace("S", "J")) // prod
			//      .map(line => line.replace("S", "F")) // test
			.zipWithIndex
			.map((line, index) => countInsiders(line, index, tiles))
			.sum
		println(res2)
	}
	
	def countInsiders(line: String, index: Int, tiles: Map[CC, Int]): Int = {
		var isInside = false
		var currentSlice = ""
		
		val insiderCount = for {col <- Range(1, line.length - 1)} yield {
			val currentSymbol = line.charAt(col)
			val tileInCycle = tiles.contains(CC(index, col))
			
			(currentSymbol, tileInCycle) match
				case (_, false) => if (isInside) 1 else 0
				case ('-', true) => 0
				case ('|', true) =>
					isInside = !isInside
					0
				case ('F', true) | ('L', true) =>
					currentSlice += currentSymbol
					0
				case ('J', true) | ('7', true) =>
					currentSlice += currentSymbol
					if (List("FJ", "L7").contains(currentSlice)) isInside = !isInside
					currentSlice = ""
					0
		}
		
		insiderCount.sum
	}
	
	val nMap = Map(
		'.' -> List.empty,
		'#' -> List.empty,
		'-' -> List(CC(0, 1), CC(0, -1)),
		'|' -> List(CC(1, 0), CC(-1, 0)),
		'F' -> List(CC(0, 1), CC(1, 0)),
		'J' -> List(CC(-1, 0), CC(0, -1)),
		'L' -> List(CC(-1, 0), CC(0, 1)),
		'7' -> List(CC(1, 0), CC(0, -1)),
		'S' -> List(CC(0, 1), CC(0, -1), CC(1, 0), CC(-1, 0))
	)
}
