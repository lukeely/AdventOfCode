package solutions.year2025

import utils.Day
import utils.Year.Year25

object day04 extends Day[Array[Array[Boolean]], Int, Int](Year25, 4) {
  private val sample =
    """..@@.@@@@.
      |@@@.@.@.@@
      |@@@@@.@.@@
      |@.@@@@..@.
      |@@.@@@@.@@
      |.@@@@@@@.@
      |.@.@.@.@@@
      |@.@@@.@@@@
      |.@@@@@@@@.
      |@.@.@@@.@.""".stripMargin

  override def parseInput(input: String): Array[Array[Boolean]] = input
    .linesIterator
    .map(line => line.toList.map(_ == '@').toArray)
    .toArray

  private def countAdjacentRolls(grid: Array[Array[Boolean]], x: Int, y: Int): Int =
    (x - 1 to x + 1).flatMap { nx =>
      (y - 1 to y + 1).map { ny =>
        !((nx == x && ny == y) || nx < 0 || ny < 0 || nx >= grid.head.length || ny >= grid.length || !grid(ny)(nx))
      }
    }.count(_.self)

  private def calculateRemovable(grid: Array[Array[Boolean]]): Seq[(Int, Int)] =
    for {
      y <- grid.indices
      x <- grid.head.indices
      if grid(y)(x) && countAdjacentRolls(grid, x, y) < 4
    } yield (x, y)


  override def partOne(grid: Array[Array[Boolean]]): Int = calculateRemovable(grid).length

  override def partTwo(grid: Array[Array[Boolean]]): Int = calculateRemovable(grid) match {
    case Seq() => 0
    case removable =>
      removable.foreach { (x, y) => grid(y)(x) = false }
      removable.length + partTwo(grid)
  }
}
