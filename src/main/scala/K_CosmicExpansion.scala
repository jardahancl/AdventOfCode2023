import scala.io.Source

object K_CosmicExpansion {
	case class CC(row: Int, col: Int)
	
	def solution() = {
		val board = Source.fromFile("src/main/resource/11_prod.txt").getLines.toList
		
		val res1 = part1(board, 2)
		println(s"Solution 11a: $res1")
		
		val res2 = part1(board, 1_000_000)
		println(s"Solution 11b: $res2")
	}
	
	def part1(board: List[String], expFactor: Int) = {
		val emptyRows = getEmptyIndices(board)
		val emptyCols = getEmptyIndices(board.transpose.map(list => list.mkString))
		val galaxies = board.zipWithIndex
			.flatMap((line, lineIndex) => line.toList.zipWithIndex
				.filter(pair => pair._1 == '#')
				.map(pair => pair._2)
				.map(colIndex => CC(lineIndex, colIndex))
			)

		val distances = for {
			g1 <- galaxies
			g2 <- galaxies
		} yield dist(g1, g2, emptyRows, emptyCols, expFactor)
		
		distances.sum / 2
	}
	
	def dist(cc1: CC, cc2: CC, emptyRows: List[Int], emptyCols: List[Int], expFactor: Int): Long = {
		val rowDist = getDist(List(cc1.row, cc2.row).min, List(cc1.row, cc2.row).max, emptyRows, expFactor)
		val colDist = getDist(List(cc1.col, cc2.col).min, List(cc1.col, cc2.col).max, emptyCols, expFactor)
		
		rowDist + colDist
	}
	
	def getDist(min: Int, max: Int, expansions: List[Int], expFactor: Int): Long = {
		Range(min, max).toList
			.map(i => if (expansions.contains(i)) expFactor else 1)
			.sum.toLong
	}
	
	def getEmptyIndices(board: List[String]) = {
		board.zipWithIndex
			.map((line, ind) => (line.indexOf('#'), ind))
			.filter((indexCross, _) => indexCross == -1)
			.map(t => t._2)
	}
}
