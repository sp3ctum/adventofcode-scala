import spire.math.UShort

// --- Day 7: Some Assembly Required ---
//
// This year, Santa brought little Bobby Tables a set of wires and bitwise logic
// gates! Unfortunately, little Bobby is a little under the recommended age
// range, and he needs help assembling the circuit.
//
// Each wire has an identifier (some lowercase letters) and can carry a 16-bit
// signal (a number from 0 to 65535). A signal is provided to each wire by a
// gate, another wire, or some specific value. Each wire can only get a signal
// from one source, but can provide its signal to multiple destinations. A gate
// provides no signal until all of its inputs have a signal.
//
// The included instructions booklet describes how to connect the parts
// together: x AND y -> z means to connect wires x and y to an AND gate, and
// then connect its output to wire z.
//
// For example:
//
// - 123 -> x means that the signal 123 is provided to wire x.
// - x AND y -> z means that the bitwise AND of wire x and wire y is provided to
// wire z.
// - p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and
// then provided to wire q.
// - NOT e -> f means that the bitwise complement of the value from wire e is
// provided to wire f.
//
// Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If,
// for some reason, you'd like to emulate the circuit instead, almost all
// programming languages (for example, C, JavaScript, or Python) provide
// operators for these gates.
//
// For example, here is a simple circuit:
//
// 123 -> x
// 456 -> y
// x AND y -> d
// x OR y -> e
// x LSHIFT 2 -> f
// y RSHIFT 2 -> g
// NOT x -> h
// NOT y -> i
// After it is run, these are the signals on the wires:
//
// d: 72
// e: 507
// f: 492
// g: 114
// h: 65412
// i: 65079
// x: 123
// y: 456
//
// In little Bobby's kit's instructions booklet (provided as your puzzle input),
// what signal is ultimately provided to wire a?

sealed abstract class Operation
case class LiteralValue     (a: UShort) extends Operation
case class DirectConnection (a: String) extends Operation
case class AndWires         (a: String, b: String) extends Operation
case class OrWires          (a: String, b: String) extends Operation
case class NotWire          (a: String) extends Operation
case class LeftShift        (a: String, b: Int) extends Operation
case class RightShift       (a: String, b: Int) extends Operation

case class Instruction(operation: Operation, targetWire: String)

object Instruction {
  def parse(input: String): Instruction = {
    val wire = "(\\w+)".r
    val number = "(\\d+)".r

    val literal = s"^${number} -> ${wire}".r
    val direct  = s"^${wire} -> ${wire}".r
    val and     = s"^${wire} AND ${wire} -> ${wire}".r
    val or      = s"^${wire} OR ${wire} -> ${wire}".r
    val not     = s"^NOT ${wire} -> ${wire}".r
    val lshift  = s"^${wire} LSHIFT ${number} -> ${wire}".r
    val rshift  = s"^${wire} RSHIFT ${number} -> ${wire}".r

    input match {
      case literal(n, w)     => Instruction(LiteralValue(ushort(n)), w)
      case direct(wa, wb)    => Instruction(DirectConnection(wa), wb)
      case and(wa, wb, wc)   => Instruction(AndWires(wa, wb), wc)
      case or(wa, wb, wc)    => Instruction(OrWires(wa, wb), wc)
      case not(wa, wb)       => Instruction(NotWire(wa), wb)
      case lshift(wa, n, wb) => Instruction(LeftShift(wa, n.toInt), wb)
      case rshift(wa, n, wb) => Instruction(RightShift(wa, n.toInt), wb)
      case input             => throw new IllegalArgumentException(s"unknown input '${input}'")
    }
  }

  private def ushort(v: String): UShort = UShort(v.toInt)
}

object LogicGateEmulator {
  type Circuit = Map[String,UShort]
  val emptyCircuit: Circuit = Map()

  def process(steps: Array[Instruction]): Circuit = {
    steps.foldLeft(emptyCircuit)(process)
  }

  private def process(circuit: Circuit, step: Instruction): Circuit = {
    val Instruction(operation, targetWire) = step
    def valueOf(wire: String): UShort = circuit(wire)

    operation match {
      case LiteralValue(a)     => circuit + (targetWire -> a)
      case DirectConnection(a) => circuit + (targetWire -> valueOf(a))
      case AndWires(a,b)       => circuit + (targetWire -> (valueOf(a) & valueOf(b)))
      case OrWires(a,b)        => circuit + (targetWire -> (valueOf(a) | valueOf(b)))
      case LeftShift(a,b)      => circuit + (targetWire -> (valueOf(a) << b))
      case RightShift(a,b)     => circuit + (targetWire -> (valueOf(a) >> b))
      case NotWire(a)          => circuit + (targetWire -> (~valueOf(a)))
    }
  }
}

object Day7Solution {
  def SolveSignalInWireA(): UShort = {
    val input = ParseInput()
    val circuit = LogicGateEmulator.process(input)
    circuit("a")
  }

  private def ParseInput(): Array[Instruction] = {
    val input = GetInput()
    input.map(Instruction.parse)
  }

  private def GetInput(): Array[String] = {
    InputReader.ReadInput("/Day7.txt").split("\n")
  }
}
