import scala.collection.immutable.List
import scala.collection.mutable
import scala.io.Source



object V_StepCounter {
	
	case class CC(row: Int, col: Int)
	
	case class V(row: Int, col: Int, step: Int)
	
	def solution() = {
		val input = Source.fromFile("src/main/resource/21_prod.txt").getLines.toList
		val input3x3 = Source.fromFile("src/main/resource/21_test_27x27.txt").getLines.toList
		val board = makeBorders(input)
		val board3x3 = makeBorders(input3x3)
		val start = getStart(board)
		
		//  val steps1 = 66 + 2 * 131 + 130
		val steps1 = 64
		val steps2 = 26_501_365
		
		  val res1 = getFinite(board, start, steps1)
//		val res1 = getFinite(board3x3, getStart(board3x3), steps1)
		println(s"Solution 1: $res1")
		
		val boardWithoutS = board.map(line => line.replace("S", "."))
		val res2: Long = getInfinite(boardWithoutS, start, steps2)
		println(s"Solution 2: $res2")
		//  625628021226274
	}
	
	
	def getInfinite(board: List[String], start: CC, steps: Int) = {
		val boardSize = board.length - 2
		val ccMin = 1
		val ccMax = boardSize
		
		val middles = List(CC(ccMax, start.col), CC(ccMin, start.col), CC(start.row, ccMax), CC(start.row, ccMin))
		val corners = List(CC(ccMin, ccMin), CC(ccMin, ccMax), CC(ccMax, ccMin), CC(ccMax, ccMax))
		
		val l = (steps - 132) / 131
		val v = (steps - 132) % 131
		//  println(s"$l $v")
		
		val centerSum = getFinite(board, start, steps)
		val middleSum: Long = middles.map(m => {
			List(
				getFinite(board, m, (steps - 66) % 131),
				if ((steps - 66) % 131 < 66 && (steps - 66) / 131 > 0) getFinite(board, m, (steps - 66) % 131 + 131) else 0,
				if (l > 0) (l / 2 + (l % 2)) * getFinite(board, m, steps - 66) else 0,
				if (l > 1) (l / 2) * getFinite(board, m, steps - 66 - 131) else 0
			).sum
		}).sum
		val cornerSum: Long = corners.map(m => {
			List(
				(l + 1) * getFinite(board, m, v),
				if (l > 0) l * getFinite(board, m, v + 131) else 0,
				if (l > 1) (l / 2).toLong * (l / 2).toLong * getFinite(board, m, steps - 132) else 0,
				if (l > 2) ((l + 1) / 2).toLong * ((l - 1) / 2).toLong * getFinite(board, m, steps - 132 - 131) else 0
			).sum
		}).sum
		
		//  println(s"$centerSum $middleSum $cornerSum")
		centerSum + middleSum + cornerSum
	}
	
	def getFinite(board: List[String], start: CC, steps: Int): Long = {
		val q = mutable.Queue(V(start.row, start.col, 0))
		var visited = Map[CC, Int]()
		
		while (q.nonEmpty) {
			val next = q.dequeue()
			val nextCC = CC(next.row, next.col)
			if (List('.', 'S').contains(board(next.row).charAt(next.col))
				&& next.step <= steps
				&& !visited.contains(nextCC)) {
				visited += (nextCC -> next.step)
				val neighs = List(CC(0, 1), CC(0, -1), CC(-1, 0), CC(1, 0))
					.map(d => CC(next.row + d.row, next.col + d.col))
					.foreach(cc => q.enqueue(V(cc.row, cc.col, next.step + 1)))
			}
		}
		val result = visited.count((_, n) => n % 2 == steps % 2).toLong
		
		result
	}
	
	def makeBorders(input: List[String]) = {
		val emptyLine = List.fill(input.head.length + 2)("*").mkString
		
		List(emptyLine) ++ input.map(line => "*" + line + "*") ++ List(emptyLine)
	}
	
	def getStart(board: List[String]) = {
		val start = board.map(line => line.indexOf("S"))
			.zipWithIndex.filter(t => t._1 != -1)
			.map(t => CC(t._2, t._1))
			.head
		println(s"Start position: $start")
		
		start
	}
	
}
