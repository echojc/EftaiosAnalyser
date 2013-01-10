package sh.echo.EftaiosAnalyser

import World._

object Program extends EftaiosAnalyser with App {
  val world = Basic
  val moves: MoveList = List((Light, null), (Light, null), (Light, null), (Dark, "L07"))
  analyse
}
