import scala.io.Source

object F_WaitForIt {
	case class Race(num: Int, time: Long, dist: Long)
	
	def solution() = {
		val input = Source.fromFile("src/main/resource/06_prod.txt").getLines.toList
		val races = processInput(input)
		val singleRace = processSingleInput(input)
		
		val res1 = races.map(race => beatRecordCount(race))
			.product
		val res2 = beatRecordCount(singleRace)
		
//		println(singleRace)
		println(res1)
		println(res2)
	}
	
	def processSingleInput(input: List[String]): Race = {
		val time = input(0).substring(10).replace(" ", "").toLong
		val distance = input(1).substring(10).replace(" ", "").toLong
		
		Race(-1, time, distance)
	}
	
	def beatRecordCount(race: Race): Long = {
		(0L to race.time).toList.count(x => (race.time - x) * x > race.dist)
	}
	
	def processInput(input: List[String]): List[Race] = {
		val times = input(0).substring(10).split(" ").filter(s => s != "").toList
		val distances = input(1).substring(10).split(" ").filter(s => s != "").toList
		
		val races = times.zip(distances).zipWithIndex
			.map((x, ind) => Race(ind, x._1.toLong, x._2.toLong))
		
		races
	}
}
