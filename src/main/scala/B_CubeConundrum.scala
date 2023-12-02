object B_CubeConundrum {
	def isColorValid(s: String): Boolean = {
		val firstNum = s.split(" ")(0).toInt
		val colorName = s.split(" ")(1)
		
		colorName match
			case "red" => firstNum <= 12
			case "green" => firstNum <= 13
			case "blue" => firstNum <= 14
	}
	
	def isPossible(str: String): Boolean = {
		val game = str.split(":")(1).trim
		val rounds = game.split(";").toList.map(s => s.trim)
		
		val result = for {
			round <- rounds
			color <- round.split(",").toList.map(s => s.trim)
		} yield isColorValid(color)
		
		return result.filter(b => b == false).isEmpty
	}
	
	def getGameNumber(line: String): Int = {
		line.split(":")(0).replaceAll("Game ", "").toInt
	}
	
	def getMultipliedCubes(line: String): Int = {
		val game = line.split(":")(1).trim
		val rounds = game.split(";").toList.map(s => s.trim)
		
		case class ColorCount(num: Int, color: String)
		val result = for {
			round <- rounds
			color <- round.split(",").toList.map(s => s.trim)
		} yield ColorCount(color.split(" ")(0).toInt, color.split(" ")(1))
		
		val maxGreen = result.filter(t => t.color == "green").map(t => t.num).max
		val maxBlue = result.filter(t => t.color == "blue").map(t => t.num).max
		val maxRed = result.filter(t => t.color == "red").map(t => t.num).max
		
		maxGreen * maxBlue * maxRed
	}
}