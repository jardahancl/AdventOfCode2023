import scala.io.Source
import scala.collection.immutable.List
import scala.collection.mutable.Queue
import scala.collection.mutable.Map

object P_TheFloorWillBeLava {
	case class Beam(row: Int, col: Int, dir: Char)
	
	case class CC(row: Int, col: Int)
	
	def solution() = {
		val input = Source.fromFile("src/main/resource/16_prod.txt").getLines.toList
		val board = input
		
		val start = Beam(0, 0, '>')
		val res1 = getBeams(start, board)
		println(res1)
		
//		val startTime = System.currentTimeMillis()
//		println(startTime)
		val starts = for {i <- Range(0, board.length)} yield {
			List(Beam(i, 0, '>'), Beam(0, i, 'v'), Beam(i, board.length - 1, '<'), Beam(board.length - 1, i, '^'))
		}
		val res2 = starts.flatten
			.map(start => getBeams(start, board))
			.max
		println(res2)
		
//		val endTime = System.currentTimeMillis()
//		println(endTime)
//		println((endTime - startTime).toDouble / 1000 / 60)
	}
	
	def getBeams(start: Beam, board: List[String]): Int = {
		var q = Queue(start)
		var beams = Map[CC, List[Char]]()
		
		while (q.nonEmpty) {
			val next = q.dequeue()
			val symbol = board(next.row).charAt(next.col)
			val cc = CC(next.row, next.col)
			
			if (beams.contains(cc))
				beams(cc) = beams(cc) ++ List(next.dir)
			else
				beams = beams + (cc -> List(next.dir))
			
			nMap((symbol, next.dir))
				.map(d => Beam(next.row + dMap(d).row, next.col + dMap(d).col, d))
				.filter(beam => isOnBoard(beam, board))
				.filter(beam => !beams.contains(CC(beam.row, beam.col)) || (beams.contains(CC(beam.row, beam.col)) && !beams(CC(beam.row, beam.col)).contains(beam.dir)))
				.foreach(beam => q.enqueue(beam))
		}
		
		beams.size
	}
	
	def isOnBoard(beam: Beam, board: List[String]): Boolean = {
		beam.row >= 0 && beam.row < board.length && beam.col >= 0 && beam.col < board.head.length
	}
	
	val dMap = Map(
		'>' -> CC(0, 1),
		'<' -> CC(0, -1),
		'^' -> CC(-1, 0),
		'v' -> CC(1, 0)
	)
	
	val nMap = Map(
		('.', '>') -> List('>'),
		('.', '<') -> List('<'),
		('.', '^') -> List('^'),
		('.', 'v') -> List('v'),
		
		('-', '>') -> List('>'),
		('-', '<') -> List('<'),
		('-', '^') -> List('>', '<'),
		('-', 'v') -> List('>', '<'),
		
		('|', '>') -> List('^', 'v'),
		('|', '<') -> List('^', 'v'),
		('|', '^') -> List('^'),
		('|', 'v') -> List('v'),
		
		('/', '>') -> List('^'),
		('/', '<') -> List('v'),
		('/', '^') -> List('>'),
		('/', 'v') -> List('<'),
		
		('\\', '>') -> List('v'),
		('\\', '<') -> List('^'),
		('\\', '^') -> List('<'),
		('\\', 'v') -> List('>')
	)
}
