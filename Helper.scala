package sh.echo

import scala.language.implicitConversions

package object EftaiosAnalyser {
  implicit def String2Point(coord: String): Point = {
    Point(coord(0).toUpper - 'A', (coord drop 1).toInt - 1)
  }

  class IntWithParity(n: Int) {
    def isEven = n % 2 == 0
    def isOdd = !isEven
  }
  implicit def Int2IntWithParity(n: Int) = new IntWithParity(n)
  implicit def Point2IntWithParity(p: Point) = new IntWithParity(p.x + p.y)
}
