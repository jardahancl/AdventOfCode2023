import scala.io.Source
import scala.collection.mutable.Queue

object Q_ClumsyCrucible {
	def solution() = {
		val startTime = System.currentTimeMillis()
		val input = Source.fromFile("src/main/resource/17_prod.txt").getLines.toList
		val emptyLine = List.fill(input.head.length + 2)(".").mkString
		val board = List(emptyLine) ++ input.map(line => "." + line + ".") ++ List(emptyLine)
		
		val res1 = part1(board)
		println(s"Solution 12a: $res1")
//		684 in 3 m 48 s (size 235_189)
		
		val res2 = part2(board)
		println(s"Solution 12b: $res2")
//		822 in 1h 1m 1s  (size 764_222)
		
		val endTime = System.currentTimeMillis()
		println(s"Duration ${(endTime - startTime) / 1000} seconds")
	}
	
	case class History(dir: Char, step: Int)
	
	case class CC(row: Int, col: Int)
	
	case class CCHistory(cc: CC, hist: History)
	
	case class Position(cc: CC, sum: Int, hist: History)
	
	def part1(board: List[String]) = {
		val start = Position(CC(1, 1), 0, History('.', 0))
		val q = Queue(start)
		var positions = Map[CCHistory, (Int, List[CC])](CCHistory(start.cc, start.hist) -> (0, List()))
		
		while (q.nonEmpty) {
			val next = q.dequeue()
			
			nMap.map(dir => CC(next.cc.row + dir.row, next.cc.col + dir.col))
				.filter(cc => board(cc.row).charAt(cc.col) != '.')
				.map(cc => Position(cc, next.sum + Integer.parseInt(board(cc.row).charAt(cc.col).toString), updateHistory(next, cc)))
				.filter(position => position.hist.step < 4)
				.filter(position => !List("UD", "DU", "LR", "RL").contains(List(position.hist.dir, next.hist.dir).mkString))
				.foreach(position => {
					val ccHist = CCHistory(position.cc, position.hist)
					if ((!positions.contains(ccHist)) || (positions.contains(ccHist) && positions(ccHist)._1 > position.sum))
						positions = positions + (ccHist -> (position.sum, positions(CCHistory(next.cc, next.hist))._2 ++ List(position.cc)))
						q.enqueue(position)
				})
		}
		
		positions.keys.filter(cch => cch.cc == CC(board.length - 2, board.length - 2))
			.map(cch => (cch, positions(cch)))
			.map(t => t._2._1)
			.min
	}
	
	def part2(board: List[String]) = {
		val startR = Position(CC(1, 1), 0, History('R', 0))
		val startD = Position(CC(1, 1), 0, History('D', 0))
		val q: Queue[Position] = Queue()
		q.enqueue(startD)
		q.enqueue(startR)
		var positions = Map[CCHistory, (Int, List[CC])](
			CCHistory(startR.cc, startR.hist) -> (0, List()),
			CCHistory(startD.cc, startD.hist) -> (0, List())
		)
		var count = 0
		
		while (q.nonEmpty) {
			val next = q.dequeue()
			
			nMap.map(dir => CC(next.cc.row + dir.row, next.cc.col + dir.col))
				.filter(cc => board(cc.row).charAt(cc.col) != '.')
				.map(cc => Position(cc, next.sum + Integer.parseInt(board(cc.row).charAt(cc.col).toString), updateHistory(next, cc)))
				.filter(position => {
					if (position.hist.dir != next.hist.dir) next.hist.step >= 4
					else true
				})
				.filter(position => position.hist.step <= 10)
				.filter(position => !List("UD", "DU", "LR", "RL").contains(List(position.hist.dir, next.hist.dir).mkString))
				.foreach(position => {
					val ccHist = CCHistory(position.cc, position.hist)
					if ((!positions.contains(ccHist)) || (positions.contains(ccHist) && positions(ccHist)._1 > position.sum))
						positions = positions + (ccHist -> (position.sum, positions(CCHistory(next.cc, next.hist))._2 ++ List(position.cc)))
						q.enqueue(position)
				})
			
			count = count + 1
			if (count % 10_000 == 0) {
				val temp = positions.keys
					.filter(cch => cch.cc == CC(25, 25))
					.filter(cch => cch.hist.step >= 4)
					.map(cch => (cch, positions(cch)))
					.map(t => t._2._1)
				val res = temp.minOption
				val posSize = temp.size
				println(f"Size: ${positions.size} with posSize $posSize and min $res")
			}
		}
		
		positions.keys
			.filter(cch => cch.cc == CC(board.length - 2, board.head.length - 2))
			.filter(cch => cch.hist.step >= 4)
			.map(cch => (cch, positions(cch)))
			.map(t => t._2._1)
			.min
	}
	
	def updateHistory(lastPosition: Position, followingCC: CC): History = {
		val dir = dMap(CC(followingCC.row - lastPosition.cc.row, followingCC.col - lastPosition.cc.col))
		val steps =
			if (dir == lastPosition.hist.dir) lastPosition.hist.step + 1
			else 1
		
		History(dir, steps)
	}
	
	val nMap = List(CC(0, 1), CC(0, -1), CC(-1, 0), CC(1, 0))
	
	val dMap = Map(
		CC(0, 1) -> 'R',
		CC(0, -1) -> 'L',
		CC(-1, 0) -> 'U',
		CC(1, 0) -> 'D'
	)
}
