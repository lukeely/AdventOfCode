package solutions.year2025

import utils.Day
import utils.Year.Year25

import scala.collection.mutable.Map as MMap

object day03 extends Day[Seq[Seq[Int]], Long, Long](Year25, 3) {
  private val sample =
    """987654321111111
      |811111111111119
      |234234234234278
      |818181911112111""".stripMargin

  override def parseInput(input: String): Seq[Seq[Int]] = input
    .linesIterator
    .map(line => line.toList.map(_.toString.toInt))
    .toSeq

  override def partOne(input: Seq[Seq[Int]]): Long = input.map { bank => findLargestJoltage(bank, 2)(2).toLong }.sum

  override def partTwo(input: Seq[Seq[Int]]): Long = input.map { bank => findLargestJoltage(bank, 12)(12).toLong }.sum

  private def findLargestJoltage(bank: Seq[Int], digits: Int): MMap[Int, String] = {
    val head = bank.head.toString
    if (bank.length == 1) return MMap[Int, String](0 -> "", 1 -> head)

    val subSet = findLargestJoltage(bank.tail, digits)
    (1 to Math.min(digits, bank.length)).foreach { d =>
      val withHead = head + subSet(d - 1)

      if (!subSet.contains(d) || subSet(d).toLong < withHead.toLong) subSet(d) = withHead
    }

    subSet
  }
}
