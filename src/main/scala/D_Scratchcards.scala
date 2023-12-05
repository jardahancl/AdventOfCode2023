import scala.io.Source

object D_Scratchcards {
	case class Scorecard(num: Int, win: List[Int], you: List[Int], int: Int)
	
	def solution() = {
		val input = Source.fromFile("src/main/resource/04_prod.txt").getLines.toList
		val scoreCards = input.map(line => processLine(line))
		
		val res1 = scoreCards.map(sc => Math.pow(2, sc.int - 1).toInt).sum
		val res2 = getSolution2(scoreCards)
		
		println(res1)
		println(res2)
	}
	
	case class PResult(sum: Int, list: List[Int])
	
	def getSolution2(scorecards: List[Scorecard]): Int = {
		val intersections = scorecards.map(sc => sc.int)
		val foldFunction = (pr: PResult, next: Int) => {
			val headVal = if (pr.list.nonEmpty) pr.list.head else 0
			val tailVal = if (pr.list.nonEmpty) pr.list.tail else List.empty
			val addedList = List.fill(next)(headVal) ++ List.fill(tailVal.length - next)(0)
			
			PResult(pr.sum + headVal, tailVal.zip(addedList).map(t => t._1 + t._2))
		}
		
		intersections
			.foldLeft(PResult(0, List.fill(intersections.length)(1)))(foldFunction)
			.sum
	}
	
	def processLine(line: String): Scorecard = {
		val num = line.split(":")(0).replace("Card ", "").trim.toInt
		val win = line.split(":")(1).split('|')(0).trim.split(" ").toList.filter(_ != "").map(_.toInt)
		val you = line.split('|')(1).trim.split(" ").toList.filter(_ != "").map(_.toInt)
		
		Scorecard(num, win, you, win.intersect(you).length)
	}
	
}
