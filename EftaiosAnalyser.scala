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

object Fermi extends World {
  val origin: Point = "H01"
  val mapStr = """--3---4--
                 |---O-#---
                 |--#O#O#--
                 |--O###O--
                 |--1-O-2--
                 |-O--O--O-
                 |-#O-O-#O-
                 |--OOOOO--
                 |-#--A--O-
                 |OOO-H-O#O
                 |O-OOOOO-#
                 |-#O-O-#O-
                 |---#-O---"""
}

abstract class EftaiosAnalyser {
  type MoveList = List[(Hex, Point)]

  val world: World
  val moves: MoveList

  /**
   * Ranged from 0.1 to 1.0, where escape pods are ranked most desirable (1.0).
   */
  lazy val hexDesirability: Map[Point, Double] = {
    def iter(list: Map[Point, Int]): Map[Point, Int] = {
      if (world.map.keys filter { h => world.has(h) && world.map(h) != AlienSpawn && world.map(h) != HumanSpawn } forall { h => list.keys exists { _ == h } }) list
      else iter(list ++ (list.keys flatMap { h => world.neighbours(h) filterNot { p => list.keys exists { _ == p } } map { p => (p, list(h) + 1) } }))
    }

    val endLocations = world.map filter { _._2.isInstanceOf[EscapePod] } map { _._1 }
    val distanceFromEnd = iter((endLocations map { _ -> 0 }).toMap)

    val max = distanceFromEnd.values.max
    distanceFromEnd map { pair => pair._1 -> (max - (pair._2 * 0.8)) / max }
  }

  def analyse {
    def iter(possibleLocations: List[Point], moves: MoveList): List[Point] = {
      if (moves.isEmpty) possibleLocations
      else {
        val currentMove = moves.head
        val hexesBasedOnType = possibleLocations flatMap { h => world.neighbours(h) filter { p => world.map(p) == currentMove._1 } }
        val locationsToSearch =
          if (currentMove._1 == Light || !hexesBasedOnType.contains(currentMove._2)) hexesBasedOnType
          else {
            // move they reported is possible, so we assign greater weighting to it
            currentMove._2 :: currentMove._2 :: hexesBasedOnType
          }
        iter(locationsToSearch, moves.tail)
      }
    }

    val initialLocation = (world.map find { _._2 == HumanSpawn }).get._1
    val intermediateResults = iter(List(initialLocation), moves)

    // find number of each id and use that as probability
    val size: Double = intermediateResults.size
    val results = intermediateResults.distinct map { h => (h, (intermediateResults count { _ == h }) / size * hexDesirability(h)) }

    // normalise results
    val total = results.foldLeft(0.0)(_ + _._2)
    val finalResults = (results map { pair => (pair._1, pair._2 / total) } sortBy { _._2 }).reverse

    println(finalResults)
  }
}

