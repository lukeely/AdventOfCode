package solutions.year2024

import utils.Day
import utils.Year.Year24

object day25 extends Day[(Seq[Seq[Int]], Seq[Seq[Int]]), Int, Int](Year24, 25) {
  def parseInput(input: String): (Seq[Seq[Int]], Seq[Seq[Int]]) = {
    val sections = input.split("\n\n")
    val (keySection, lockSection) = sections.partition(_.take(5) == ".....")

    val Seq(keys, locks) = Seq(keySection, lockSection).map(section =>
      section.map(section => section
          .linesIterator
          .map(line => line.toList)
          .toSeq
          .transpose
          .map(_.count(_ == '#') - 1))
          .toSeq
    )

    (keys, locks)
  }

  def partOne(input: (Seq[Seq[Int]], Seq[Seq[Int]])): Int = input match
    case (keys, locks) => keys.map(key => locks.count(lock => key.fitsLock(lock))).sum
}

extension (key: Seq[Int])
  def fitsLock(lock: Seq[Int]): Boolean = key.zip(lock).count((k, l) => k + l >= 6) == 0
