import scala.io.Source

object L_HotSprings {
	case class Lake(map: String, record: List[Int])
	
	def solution() = {
		val startTime = System.currentTimeMillis()
		val input = Source.fromFile("src/main/resource/12_test.txt").getLines.toList
		val lakes = input.map(line => {
			Lake(line.split(' ')(0), line.split(' ')(1).split(',').toList.map(x => x.toInt))
		})
		
		val res1 = lakes.zipWithIndex.map((lake, ind) => countSolutionsSlow(lake)).sum
		println(s"Solution 12a: $res1")
		
		val extLakes = lakes.map(lake => Lake(
			List.fill(4)(lake.map + "?").mkString + lake.map,
			List.fill(5)(lake.record).flatten
		))
		println(extLakes)
		val res2 = extLakes.zipWithIndex.map((lake, ind) => countSolutionsFast(lake)).sum
		println(s"Solution 12b: $res2")

		val endTime = System.currentTimeMillis()
		println(s"Duration ${(endTime - startTime) / 1000} seconds")
	}
	
	def countSolutionsFast(lake: Lake): Long = {
		def recordSplitted = lake.record.splitAt(lake.record.length / 2)
		
		def minPosition = recordSplitted._1.sum + recordSplitted._1.length - 1
		
		def maxPosition = lake.map.length - (recordSplitted._2.sum + recordSplitted._2.length - 1)
		
		val partialSolutionCount = for pos <- Range(minPosition, maxPosition) yield {
			if (lake.map.charAt(pos) != '#'
				&& !lake.map.substring(pos + 1, pos + 1 + recordSplitted(1).head).contains('.')
				&& lake.map.length > pos + recordSplitted(1).head + 1
				&& lake.map.charAt(pos + recordSplitted(1).head + 1) != '#') {
				val leftSolutionCount = countSolutionsSlow(Lake(lake.map.substring(0, pos), recordSplitted(0)))
				val rightSolutionCount =
					if (recordSplitted(1).tail.nonEmpty) countSolutionsSlow(Lake(lake.map.substring(pos + 1 + recordSplitted(1).head + 1), recordSplitted(1).tail))
					else if (!lake.map.substring(pos + 1 + recordSplitted(1).head).contains('#')) 1L
					else 0L
				
				leftSolutionCount * rightSolutionCount
			}
			else 0
		}
		
		partialSolutionCount.sum
	}
	
	def countSolutionsSlow(lake: Lake): Long = {
		val firstCrossIndex = if (lake.map.indexOf('#') == -1) Int.MaxValue else lake.map.indexOf('#')
		val firstQuestionIndex = if (lake.map.indexOf('?') == -1) Int.MaxValue else lake.map.indexOf('?')
		val firstUnresolvedIndex = List(firstCrossIndex, firstQuestionIndex).min
		
		if (firstUnresolvedIndex == Int.MaxValue) {
			if (lake.record.isEmpty) return 1
			else return 0
		}
		
		val crossSolutionCount = {
			if (lake.record.isEmpty) 0
			else if (lake.record.sum > lake.map.length - lake.map.count(ch => ch == '.')) 0
			else if (lake.record.map(n => n + 1).sum - 1 > lake.map.length) 0
			else if (lake.map.length < firstUnresolvedIndex + lake.record.head) 0
			else if (lake.map.substring(firstUnresolvedIndex, firstUnresolvedIndex + lake.record.head).contains('.')) 0
			else if (lake.map.length < firstUnresolvedIndex + lake.record.head + 1) 1
			else if (lake.map.charAt(firstUnresolvedIndex + lake.record.head) == '#') 0
			else countSolutionsSlow(Lake(lake.map.substring(firstUnresolvedIndex + lake.record.head + 1), lake.record.tail))
		}
		
		val dotSolutionCount = if (firstUnresolvedIndex == firstQuestionIndex) {
			countSolutionsSlow(Lake(lake.map.substring(firstUnresolvedIndex + 1), lake.record))
		} else 0
		
		dotSolutionCount + crossSolutionCount
	}
}
