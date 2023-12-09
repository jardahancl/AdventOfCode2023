import scala.io.Source

object H_HauntedWasteland {
	case class Node(id: String, left: String, right: String)
	
	case class State(node: Node, var finishes: Map[Node, List[Int]], var cycle: (Int, Int))
	
	case class Cycle(start: Int, end: Int, len: Int, prePeriod: List[Int], period: List[Int])
	
	def solution() = {
		val input = Source.fromFile("src/main/resource/08_prod.txt").getLines.toList
		val instructions = input.head
		val nodes = getNodes(input)
		
		val res1 = goAToZ(instructions, nodes)
		
		val positionsA = nodes.values.filter(n => n.id.endsWith("A")).toList
		val result = for startPosition <- positionsA yield getState(startPosition, instructions, nodes)
		val cycles = result
			.map(st => (st, st.finishes.values.flatten.toList.distinct))
			.map(x => Cycle(x._1.cycle._1, x._1.cycle._2, x._1.cycle._2 - x._1.cycle._1, x._2,
				x._2.filter(n => n >= x._1.cycle._1).map(n => n - x._1.cycle._1)))
		val res2 = lcm(cycles.map(_.start))
		
		println(res1)
		println(res2)
	}
	
	def getState(position: Node, instructions: String, nodes: Map[String, Node]) = {
		val positionsZ = nodes.values.filter(n => n.id.endsWith("Z")).toList
		val iLen = instructions.length
		
		var step = 0
		var current = position
		var state = State(position, Map(), (0, 0))
		while (state.cycle == (0, 0)) {
			//check for ending "Z"
			if (positionsZ.contains(current)) {
				val finishes = if (state.finishes.contains(current)) {
					if (state.finishes(current).map(st => st % iLen).contains(step % iLen))
						state.cycle = (state.finishes(current).filter(st => st % iLen == step % iLen).head, step)
					val updatedList = state.finishes(current) ++ List(step)
					state.finishes + (current -> updatedList)
				} else state.finishes ++ Map(current -> List(step))
				
				state = State(position, finishes, state.cycle)
			}
			// next step
			current =
				if (instructions.charAt(step % instructions.length) == 'L') nodes(current.left)
				else nodes(current.right)
			step = step + 1
		}
		
		state
	}
	
	def goAToZ(instructions: String, nodes: Map[String, Node]) = {
		var step = 0
		var position = nodes("AAA")
		
		while (position != nodes("ZZZ")) {
			if (instructions.charAt(step % instructions.length) == 'L') position = nodes(position.left)
			else position = nodes(position.right)
			step = step + 1
		}
		
		step
	}
	
	def getNodes(input: List[String]) = {
		input.tail.tail
			.map(line => Node(line.substring(0, 3), line.substring(7, 10), line.substring(12, 15)))
			.map(node => (node.id, node))
			.toMap
	}
	
	def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt) { (a, b) => b * a / Stream.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs }
	
}
