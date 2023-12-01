import scala.io.Source

@main
def main(): Unit = {
  val allNumbers = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  val input = Source.fromFile("src/main/resource/01_prod.txt").getLines.toList
  
  def getNumber(line: String): Int = {
    val lastNum = line.toList.foldLeft(0)((x, y) => if (y.isDigit) y.asDigit else x)
    val firstNum = line.toList.foldRight(0)((x, y) => if (x.isDigit) x.asDigit else y)
    
    10 * firstNum + lastNum
  }
  
  def getAllNumber(line: String): Int = {
    val lastNumber = allNumbers
        .map(s => (s, line.lastIndexOf(s)))
        .foldLeft(("", Int.MinValue))((x, y) => if (x._2 < y._2) y else x)
    val lastValue = (allNumbers.indexOf(lastNumber._1) + 1) % 10
  
    val firstNumber = allNumbers
        .map(s => (s, line.indexOf(s)))
        .filter(t => t._2 != -1)
        .foldLeft(("", Int.MaxValue))((x, y) => if (x._2 < y._2 ) x else y)
    val firstValue = (allNumbers.indexOf(firstNumber._1) + 1) % 10
  
    10 * firstValue + lastValue
  }
  
  val res1 = input.map(line => getNumber(line)).sum
  val res2 = input.map(line => getAllNumber(line)).sum
  
  println(res1)
  println(res2)
}