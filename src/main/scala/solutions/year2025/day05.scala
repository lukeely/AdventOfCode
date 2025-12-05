package solutions.year2025

import utils.Day
import utils.Year.Year25

object day05 extends Day[(Seq[(Long, Long)], Seq[Long]), Int, Long](Year25, 5) {
  private val sample =
    """3-5
      |10-14
      |16-20
      |12-18
      |
      |1
      |5
      |8
      |11
      |17
      |32""".stripMargin

  override def parseInput(input: String): (Seq[(Long, Long)], Seq[Long]) = {
    val Seq(ranges, ids) = input.split("\n\n").toSeq

    val parsedRanges = ranges.linesIterator.map { line =>
      line.split("-").toSeq match
        case Seq(start, end) => (start.toLong, end.toLong)
    }.toSeq

    (parsedRanges, ids.linesIterator.map(_.toLong).toSeq)
  }

  override def partOne(input: (Seq[(Long, Long)], Seq[Long])): Int = {
    val (ranges, ids) = input
    ids.count { id => ranges.exists { (start, end) => id >= start && id <= end } }
  }

  private def countValidIds(ranges: Seq[(Long, Long)]): Long = ranges match
    case Seq() => 0
    case (start1, end1) :: (start2, end2) :: rest if start2 <= end1 => countValidIds((start1, end2) +: rest)
    case (start, end) :: rest => end - start + 1 + countValidIds(rest)

  override def partTwo(input: (Seq[(Long, Long)], Seq[Long])): Long = {
    val (starts, ends) = input._1.unzip
    countValidIds(starts.sorted.zip(ends.sorted))
  }
}
