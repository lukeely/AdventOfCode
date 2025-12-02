package solutions.year2017

import utils.Day
import utils.Year.Year17

object day10 extends Day[Seq[Int], Int, Int](Year17, 10) {

  override def parseInput(input: String): Seq[Int] = input.split(",").map(_.toInt)

  override def partOne(input: Seq[Int]): Int = ???
}
