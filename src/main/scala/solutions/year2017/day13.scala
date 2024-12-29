package solutions.year2017

import utils.Day
import utils.Year.Year17

import scala.annotation.tailrec

object day13 extends Day[Seq[Int], Int, Int](Year17, 13) {
  private val depthReg = "(\\d+): (\\d+)".r

  def parseInput(input: String): Seq[Int] = {
    val depths = input.linesIterator.map(line =>
      val List(index, depth) = depthReg.findFirstMatchIn(line).get.subgroups.map(_.toInt)
      (index, depth)
    ).toMap
    (0 to depths.keySet.max).map(index => depths.getOrElse(index, 0))
  }

  def getPosition(depth: Int, time: Int): Int = time % (depth * 2 - 2) match
    case x if x < depth => x
    case x => depth - (x % (depth - 1)) - 1

  def partOne(depths: Seq[Int]): Int = depths
    .zipWithIndex
    .filter((depth, time) => getPosition(depth, time) == 0)
    .map((depth, range) => depth * range)
    .sum

  @tailrec
  private def findPassingTime(depths: Seq[Int], offSet: Int): Int =
    if (depths.zipWithIndex.count((depth, index) => getPosition(depth, index + offSet) == 0) == 0)
      offSet else findPassingTime(depths, offSet + 1)

  override def partTwo(depths: Seq[Int]): Int = findPassingTime(depths, 0)
}
