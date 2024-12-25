package solutions.year2024

import solutions.year2024.day24.Gate
import utils.Day
import utils.SequenceUtils.flip
import utils.Year.Year24

import scala.annotation.tailrec

object day24 extends Day[(Map[String, Boolean], Seq[Gate]), Long, String](Year24, 24) {
  enum Operation:
    case AND
    case OR
    case XOR

  private object Operation {
    def fromName(name: String): Operation = name match
      case "XOR" => XOR
      case "OR" => OR
      case "AND" => AND
  }

  case class Gate(inputOne: String, inputTwo: String, operation: Operation, result: String) {
    def compute(inputs: Map[String, Boolean]): Option[Boolean] = {
      if (!inputs.contains(inputOne) || !inputs.contains(inputTwo)) return None
      (inputs(inputOne), inputs(inputTwo), operation) match
        case (a, b, Operation.AND) => Some(a && b)
        case (a, b, Operation.OR) => Some(a || b)
        case (true, true, Operation.XOR) => Some(false)
        case (a, b, Operation.XOR) => Some(a || b)
    }

    def swapWire(swaps: Map[String, String]): Gate =
      Gate(inputOne, inputTwo, operation, swaps.getOrElse(result, result))
  }

  private val wireInputReg = "([^:]+): ([10])".r
  private val gateInputReg = "(\\w+) (AND|OR|XOR) (\\w+) -> (\\w+)".r

  def parseInput(input: String): (Map[String, Boolean], Seq[Gate]) = {
    val Seq(wiresInput, gatesInput) = input.split("\n\n").toSeq
    val wires = wiresInput.linesIterator.map(line =>
      val List(name, value) = wireInputReg.findFirstMatchIn(line).get.subgroups
      (name, value == "1")
    ).toMap

    val gates = gatesInput.linesIterator.map(line =>
      val List(inputOne, operation, inputTwo, result) = gateInputReg.findFirstMatchIn(line).get.subgroups

      Gate(inputOne, inputTwo, Operation.fromName(operation), result)
    ).toSeq

    (wires, gates)
  }

  @tailrec
  private def computeAllWires(wires: Map[String, Boolean], gates: Seq[Gate]): Map[String, Boolean] = {
    if (gates.isEmpty) return wires

    val results = gates.map(gate => (gate, gate.compute(wires)))
    val newWires = results.filter(_._2.isDefined).map((gate, result) => (gate.result, result.get))
    computeAllWires(wires ++ newWires, results.filter(_._2.isEmpty).map(_._1))
  }

  private def computeOutputWires(wires: Map[String, Boolean], start: Char = 'z'): Long =
    wires.toSeq.filter(_._1.head == start).sortBy(_._1).reverse.map(_._2).asBinaryString().fromBinary()

  private def computeCorrectResult(wires: Map[String, Boolean]): Long =
    computeOutputWires(wires, 'x') + computeOutputWires(wires, 'y')

  private def findConnectedWires(wires: Set[String], gates: Seq[Gate]): Set[String] = {
    val nonInputs = wires.filter(wire => wire.head != 'x' && wire.head != 'y')
    if (nonInputs.isEmpty) return nonInputs

    val nextInputs = nonInputs
      .map(result => gates.find(gate => gate.result == result).get)
      .flatMap(gate => Set(gate.inputOne, gate.inputTwo))

    nonInputs ++ findConnectedWires(nextInputs, gates)
  }

  private def swapWires(swaps: Map[String, String], gates: Seq[Gate]): Seq[Gate] = gates.map(_.swapWire(swaps))

  def isInputWire(wire: String): Boolean = wire.head == 'x' || wire.head == 'y'

  override def partOne(input: (Map[String, Boolean], Seq[Gate])): Long = input match
    case (wires, gates) => computeOutputWires(computeAllWires(wires, gates))

  override def partTwo(input: (Map[String, Boolean], Seq[Gate])): String = input match
    case (wires, gates) => {
      val swaps = Seq(("hdt", "z05"), ("gbf", "z09"), ("nbf", "z30"), ("mht", "jgt"))
      val swapMap = (flip(swaps) ++ swaps).toMap
      val swappedWires = swapWires(swapMap, gates)


//      (1 to 44).foreach(num =>
//        val padded = num.toString.reverse.padTo(2, '0').reverse
//        val inputs = swappedWires.withInputs(s"x$padded", s"y$padded").sortBy(_.operation.toString)
//        val allGates = inputs.flatMap(gate => swappedWires.withInputs(gate.result)).sortBy(_.operation.toString)
//        println(inputs ++ allGates)
//      )

      swaps.flatMap((a, b) => Seq(a, b)).sorted.mkString(",")
    }
}

extension (bin: Seq[Boolean])
  def asBinaryString(): String = bin.map(b => if (b) 1 else 0).mkString

extension (bin: String)
  def fromBinary(): Long = {
    if (bin.isEmpty) return 0L

    val front = bin.take(30)
    val back = bin.drop(30)
    Integer.parseInt(front, 2).toLong * Math.pow(2, back.size).toLong + back.fromBinary()
  }

extension (gates: Seq[Gate])
  def withInputs(a: String, b: String): Seq[Gate] = gates.withInputs(a, Some(b))

  def withInputs(a: String, b: Option[String] = None): Seq[Gate] = b match
    case Some(b) => gates.filter(gate => (gate.inputOne == a && gate.inputTwo == b) || (gate.inputOne == b && gate.inputTwo == a))
    case _ => gates.filter(gate => gate.inputOne == a || gate.inputTwo == a)
