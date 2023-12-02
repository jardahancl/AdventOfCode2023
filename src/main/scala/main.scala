import scala.io.Source
//import B_CubeConundrum

@main
def main(): Unit = {
//  solutionTrebuchet()
  solutionCubeConundrum()
  
}

def solutionCubeConundrum() = {
  val input = Source.fromFile("src/main/resource/02_prod.txt").getLines.toList
  
  val res1 = input.filter(line => B_CubeConundrum.isPossible(line))
      .map(line => B_CubeConundrum.getGameNumber(line))
      .sum
  val res2 = input.map(line => B_CubeConundrum.getMultipliedCubes(line))
      .sum
  
  println(res1)
  println(res2)
}

def solutionTrebuchet() = {
  val input = Source.fromFile("src/main/resource/01_prod.txt").getLines.toList
  
  val res1 = input.map(line => A_Trebuchet.getNumber(line))
      .sum
  val res2 = input.map(line => A_Trebuchet.getAllNumber(line))
      .sum
  
  println(res1)
  println(res2)
}
