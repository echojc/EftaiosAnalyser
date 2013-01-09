package sh.echo.EftaiosAnalyser

case class Point(x: Int, y: Int) {
  override def toString = f"${x + 'A'}%c${y + 1}%02d"
}

object World {
  abstract trait Hex { def mkString = " " }
  case object Empty extends Hex
  case object Light extends Hex
  case object Dark extends Hex { override def mkString = "#" }
  case class EscapePod(n: Int) extends Hex { override def mkString = n.toString }
  case object AlienSpawn extends Hex { override def mkString = "A" }
  case object HumanSpawn extends Hex { override def mkString = "H" }
}

trait World {
  import World._

  val origin: Point
  val mapStr: String

  lazy val world = mapStr.stripMargin('|').split('\n')
  lazy val width = world(0).size
  lazy val height = world.size

  lazy val map: Map[Point, Hex] = {
    (for {
      y <- 0 until height
      x <- 0 until width
    } yield Point(x + origin.x, y + origin.y) -> (world(y)(x) match {
      case 'O' => Light
      case '#' => Dark
      case 'A' => AlienSpawn
      case 'H' => HumanSpawn
      case n @ ('1' | '2' | '3' | '4') => EscapePod(n.toString.toInt)
      case _ => Empty
    })).toMap
  }

  def neighbours(p: Point) = {
    val x = p.x
    val y = p.y
    val points = List(Point(x - 1, y), Point(x + 1, y), Point(x, y - 1), Point(x, y + 1)) ++
      (if (x % 2 == 0) List(Point(x - 1, y - 1), Point(x + 1, y - 1))
      else List(Point(x - 1, y + 1), Point(x + 1, y + 1)))
    points filter { p => has(p) && map(p) != AlienSpawn && map(p) != HumanSpawn }
  }

  def has(p: Point) = (map contains p) && (map(p) != Empty)

  def draw {
    def drawTopRow(y: Int) {
      for (x <- origin.x to (origin.x + width)) {
        val point = Point(x, y)
        if (x.isEven) {
          if (has(Point(x - 1, y - 1)) || has(point)) print("/") else print(" ")
          if (has(point)) print(map(point).mkString) else print(" ")
        } else {
          if (has(Point(x - 1, y)) || has(Point(x, y - 1))) print("\\") else print(" ")
          if (has(Point(x, y - 1)) || has(point)) print("_") else print(" ")
        }
      }
      println
    }

    def drawBottomRow(y: Int) {
      for (x <- origin.x to (origin.x + width)) {
        val point = Point(x, y)
        if (x.isEven) {
          if (has(Point(x - 1, y)) || has(point)) print("\\") else print(" ")
          if (has(Point(x, y + 1)) || has(point)) print("_") else print(" ")
        } else {
          if (has(Point(x - 1, y)) || has(point)) print("/") else print(" ")
          if (has(point)) print(map(point).mkString) else print(" ")
        }
      }
      println
    }

    for (y <- origin.y to (origin.y + height)) {
        drawBottomRow(y - 1)
        drawTopRow(y)
    }
  }
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
