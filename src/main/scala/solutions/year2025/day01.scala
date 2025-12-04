package solutions.year2025

import utils.Day
import utils.Year.Year25

enum Direction:
  case Right
  case Left

  def convertDegrees(degrees: Int): Int = this match
    case Right => degrees
    case Left => -degrees

object Direction {
  def fromChar(input: Char): Direction = input match
    case 'R' => Right
    case 'L' => Left
}

case class Turn(direction: Direction, degree: Int)

object day01 extends Day[Seq[Turn], Int, Int](Year25, 1) {
  private val totalDegrees = 100
  private val startingDegree = 50

  private val sample =
    """L68
      |L30
      |R48
      |L5
      |R60
      |L55
      |L1
      |L99
      |R14
      |L82""".stripMargin


  override def parseInput(input: String): Seq[Turn] = input
    .linesIterator
    .map(line => Turn(Direction.fromChar(line.head), line.tail.toInt))
    .toSeq

  private def positions(input: Seq[Turn]): Seq[(Int, Int)] = input
    .map(turn => turn.direction.convertDegrees(turn.degree))
    .scanLeft((startingDegree, 0)) { case ((current, _), change) =>
      val next = current + change
      val passes = (if (change > 0 || current == 0) next else totalDegrees - next).abs / totalDegrees

      (Math.floorMod(next, totalDegrees), passes)
    }

  override def partOne(input: Seq[Turn]): Int = positions(input).count((position, _) => position == 0)

  override def partTwo(input: Seq[Turn]): Int = positions(input).map((_, passes) => passes).sum
}
