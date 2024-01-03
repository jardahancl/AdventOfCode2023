import scala.io.Source

object R_LavaductLagoon {
	
	case class CC(row: Long, col: Long)
	
	case class Instruction(dir: Char, step: Long)
	
	def solution() = {
		val input = Source.fromFile("src/main/resource/18_prod.txt").getLines.toList
		
		val instructions = input.map(line => Instruction(line.charAt(0), line.split(" ")(1).toLong))
		val res1 = part1(instructions)
		println(s"Solution 1: $res1")
//		52035
		val newInstructions = input
			.map(line => line.split(" ")(2).substring(2, 8))
			.map(newline => Instruction("RDLU".charAt(newline.charAt(5).toString.toInt), Integer.parseInt(newline.substring(0, 5), 16).toLong))
		val res2 = part1(newInstructions)
		println(s"Solution 2: $res2")
//		60612092439765
	}
	
	def part1(instructions: List[Instruction]) = {
		var current = CC(0L, 0L)
		var corners: List[CC] = List()
		val borderPointCount = instructions.map(i => i.step).sum
		
		instructions.foreach(i => {
			corners = corners ++ List(current)
			current = CC(current.row + dMap(i.dir).row * i.step, current.col + dMap(i.dir).col * i.step)
		})
		val rowMax = corners.map(c => c.row).max
		
		val areaInside = instructions.zip(corners).foldLeft(0L)((currentArea, t) => {
			val addedArea: Long = t._1 match
				case Instruction('R', len) => (len) * (rowMax - t._2.row)
				case Instruction('L', len) => (-len) * (rowMax - t._2.row)
				case _ => 0L
//			println("CurrentArea: " + currentArea.toString + ", added " + addedArea.toString + ", instruction: " + t._1.toString + ", cc: " + t._2)
			
			currentArea + addedArea
		})
		
		areaInside + borderPointCount / 2 + 1 // From Pick Theorem
	}
	
	val dMap = Map(
		'R' -> CC(0, 1),
		'L' -> CC(0, -1),
		'U' -> CC(-1, 0),
		'D' -> CC(1, 0)
	)
	
}
