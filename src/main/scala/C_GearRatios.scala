import scala.io.Source
import scala.collection.mutable.ListBuffer


object C_GearRatios {
	def solution() = {
		val input = Source.fromFile("src/main/resource/03_prod.txt").getLines.toList
		
		val res1 = getPartNumbers(input)
		val res2 = getGearSymbols(input)
		
		println(res1)
		println(res2)
		// 81296565 to low
		// 81289676 to low
		// 81997870 correct
	}
	
	case class Symbol(s: Char, row: Int, col: Int)
	
	case class Number(n: Int, line: String, col: Int, len: Int, sym: Symbol)
	
	case class Point(row: Int, col: Int)
	
	def getGearSymbols(input: List[String]): Int = {
		val allNumbers = input.zipWithIndex
			.flatMap((line, lineCount) => extractNumbers(line, input(lineCount)))
		val matchedNumbers = allNumbers
			.map(number => matchNumber(number, input))
		
		val allGearSymbols = matchedNumbers
			.map(n => n.sym)
			.filter(sb => sb.s == '*')
		val symbolsTwice = allGearSymbols
			.groupBy(identity)
			.mapValues(_.size)
			.filter(m => m._2 >= 2)
			.keys.toList
		val products = symbolsTwice
			.map(s => matchedNumbers
				.filter(mn => mn.sym == s)
				.map(mn => mn.n).product
			)
		
		
		products.sum
	}
	
	
	def getPartNumbers(input: List[String]): Int = {
		val allNumbers = input.zipWithIndex
			.flatMap((line, lineCount) => extractNumbers(line, input(lineCount)))
		val matchedNumbers = allNumbers
			.map(number => matchNumber(number, input))
		
		matchedNumbers
			.filter(number => number.sym.s != '.')
			.map(number => number.n)
			.sum
	}
	
	def extractNumbers(line: String, lineCount: String): List[Number] = {
		val regex = raw"\d+".r
		val numbersFinds = regex.findAllMatchIn(line).toList
		
		numbersFinds.map(x => Number(line.substring(x.start, x.end).toInt, lineCount, x.start, line.substring(x.start, x.end).length, Symbol('.', -1, -1)))
	}
	
	def getNeigborsPosition(number: Number, input: List[String]): List[Point] = {
		val lineNumber = input.indexOf(number.line)
		val colNumber = number.col
		var resultPositions = new ListBuffer[Point]()
		
		// corners
		if (lineNumber - 1 >= 0 && colNumber - 1 >= 0) resultPositions += Point(lineNumber - 1, colNumber - 1)
		if (lineNumber - 1 >= 0 && colNumber + number.len < number.line.length) resultPositions += Point(lineNumber - 1, colNumber + number.len)
		if (lineNumber + 1 < input.length && colNumber - 1 >= 0) resultPositions += Point(lineNumber + 1, colNumber - 1)
		if (lineNumber + 1 < input.length && colNumber + number.len < number.line.length) resultPositions += Point(lineNumber + 1, colNumber + number.len)
		// lines
		if (lineNumber - 1 >= 0) resultPositions.appendAll(
			for {col <- Range(colNumber, colNumber + number.len)}
				yield Point(lineNumber - 1, col))
		if (lineNumber + 1 < input.length) resultPositions.appendAll(
			for {col <- Range(colNumber, colNumber + number.len)}
				yield Point(lineNumber + 1, col))
		// walls
		if (colNumber - 1 >= 0) resultPositions += Point(lineNumber, colNumber - 1)
		if (colNumber + number.len < number.line.length) resultPositions += Point(lineNumber, colNumber + number.len)
		
		resultPositions.toList
	}
	
	def matchNumber(number: Number, input: List[String]): Number = {
		val neigh = getNeigborsPosition(number, input)
			.map(point => Symbol(input(point.row).charAt(point.col), point.row, point.col))
		
		val symbolList = neigh.filter(x => x.s != '.')
		val symbol = if (symbolList.nonEmpty) symbolList.head else Symbol('.', -1, -1)
		
		Number(number.n, number.line, number.col, number.len, symbol)
	}
	
}
