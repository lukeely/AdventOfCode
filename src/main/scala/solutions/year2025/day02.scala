package solutions.year2025

import utils.Day
import utils.Year.Year25

object day02 extends Day[Seq[(String, String)], Long, Long](Year25, 2) {
  private val sample = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

  private val partOnePattern = "^(\\d+)\\1$".r
  private val partTwoPattern = "^(\\d+)\\1+$".r

  override def parseInput(input: String): Seq[(String, String)] = input
    .split(",")
    .map(range => range.split("-").toSeq)
    .map { case Seq(a, b) => (a, b) }

  private def generateAllIds(input: Seq[(String, String)]): Seq[String] =
    input.flatMap((start, end) => (start.toLong to end.toLong).map(_.toString))

  override def partOne(input: Seq[(String, String)]): Long = generateAllIds(input)
    .filter(id => partOnePattern.matches(id))
    .map(_.toLong)
    .sum

  override def partTwo(input: Seq[(String, String)]): Long = generateAllIds(input)
    .filter(id => partTwoPattern.matches(id))
    .map(_.toLong)
    .sum
}

