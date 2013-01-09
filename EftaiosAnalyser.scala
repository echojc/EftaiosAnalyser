package sh.echo.EftaiosAnalyser

import World._

object Basic extends World {
  val origin: Point = "J02"
  val mapStr = """--#--
                 |OOHOO
                 |##O##
                 |#OOO#
                 |##O##
                 |O###O
                 |-###-
                 |1#A#2"""
}

object EftaiosAnalyser extends App {
  def analyse(world: World, moves: List[(Hex, Point)]): List[(Point, Double)] = {
    def iter(possibleLocations: List[Point], moves: List[(Hex, Point)]): List[Point] = {
      if (moves.isEmpty) possibleLocations
      else {

        iter(possibleLocations.flatMap(h => world.neighbours(h) filter { p => world.map(p) == moves.head._1 }), moves.tail)
      }
    }

    val initialLocation = (world.map find { _._2 == HumanSpawn }).get._1
    val results = iter(List(initialLocation), moves)

    // find number of each id and use that as probability
    val size: Double = points.size
    (results.distinct map { h => (h, (results count { _ == h }) / size) } sortBy { _._2 }).reverse
  }

  println(analyse(Basic, List((Light, null), (Light, null), (Light, null), (Light, null))))
}
