import scala.io.Source

object I_MirageMaintenance {
	case class Alt(sign: Int, res: Int)
	
	case class Result(heads: List[Int], tails: List[Int])
	
	def solution() = {
		val input = Source.fromFile("src/main/resource/09_prod.txt").getLines.toList
		val lines = input.map(line => line.split(" ").map(_.toInt).toList)
		
		val res = lines.map(line => getNext(line))
		val res1 = res.map(_.tails.sum)
			.sum
		val res2 = res.map(_.heads)
			.map(heads => heads.foldLeft(Alt(1, 0))((alt, el) => Alt(-alt.sign, alt.res + alt.sign * el)).res)
			.sum
		
		println(res1)
		println(res2)
	}
	
	def getNext(seq: List[Int]): Result = {
		var current = seq
		var tails: List[Int] = List()
		var heads: List[Int] = List()
		while (current.distinct.size != 1 || current.head != 0) {
			tails = tails ++ List(current.last)
			heads = heads ++ List(current.head)
			current = current.zip(current.tail)
				.map((cur, pre) => pre - cur)
		}
		
		Result(heads, tails)
	}
}
