import spire.math.UShort
import scala.annotation.tailrec

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

sealed abstract class WireOrNumberRef
case class WireRef(name: String) extends WireOrNumberRef
case class NumberRef(value: UShort) extends WireOrNumberRef

sealed abstract class Operation
case class LiteralValue     (a: UShort) extends Operation
case class DirectConnection (a: String) extends Operation
case class AndOperation     (a: WireOrNumberRef, b: WireOrNumberRef) extends Operation
case class OrWires          (a: String, b: String) extends Operation
case class NotWire          (a: String) extends Operation
case class LeftShift        (a: String, b: Int) extends Operation
case class RightShift       (a: String, b: Int) extends Operation

object Circuit {
  def parse(input: String): (String,Operation) = {
    val literal = s"^${number} -> ${wire}".r
    val direct  = s"^${wire} -> ${wire}".r
    val and     = s"^${wireOrNumber} AND ${wireOrNumber} -> ${wire}".r
    val or      = s"^${wire} OR ${wire} -> ${wire}".r
    val not     = s"^NOT ${wire} -> ${wire}".r
    val lshift  = s"^${wire} LSHIFT ${number} -> ${wire}".r
    val rshift  = s"^${wire} RSHIFT ${number} -> ${wire}".r

    input match {
      case literal(n, w)     => (w  -> LiteralValue(ushort(n)))
      case direct(wa, wb)    => (wb -> DirectConnection(wa))
      case and(wa, wb, wc)   => (wc -> AndOperation(parseWireOrNumber(wa), parseWireOrNumber(wb)))
      case or(wa, wb, wc)    => (wc -> OrWires(wa, wb))
      case not(wa, wb)       => (wb -> NotWire(wa))
      case lshift(wa, n, wb) => (wb -> LeftShift(wa, n.toInt))
      case rshift(wa, n, wb) => (wb -> RightShift(wa, n.toInt))
      case input             => throw new IllegalArgumentException(s"unknown input '${input}'")
    }
  }

  private def parseWireOrNumber(input: String): WireOrNumberRef = {
    input match {
      case wire(name) => WireRef(name)
      case number(value) => NumberRef(UShort(value.toInt))
    }
  }

  private val wire = "([a-z]+)".r
  private val number = "([0-9]+)".r
  private val wireOrNumber = "([a-z0-9]+)".r

  private def ushort(v: String): UShort = UShort(v.toInt)
}

object LogicGateEmulator {
  type WireName = String
  type UnsolvedCircuit = List[(WireName,Operation)]
  type SolvedCircuit = Map[WireName,UShort]

  // quick and dirty beginner memoization
  import scala.collection.mutable
  private[this] val vals = mutable.Map.empty[Operation,UShort]
  def cache(result: (Operation,UShort)) = vals + result

  def process(input: UnsolvedCircuit): SolvedCircuit = {
    val references = input.toMap

    def wireValue(operation: Operation): UShort = {
      if (vals.contains(operation)) {
        vals(operation)
      }
      else {
        def ref(name: String): Operation = {
          references(name)
        }

        def op(ref: WireOrNumberRef): Operation = {
          ref match {
            case WireRef(name) => {
              references(name)
            }
            case NumberRef(value) => {
              LiteralValue(value)
            }
          }
        }

        val retval = operation match {
          case LiteralValue(v)        => v
          case DirectConnection(next) => wireValue(ref(next))
          case AndOperation(a, b)     => wireValue(op(a)) & wireValue(op(b))
          case OrWires(a, b)          => wireValue(ref(a)) | wireValue(ref(b))
          case NotWire(a)             => ~ wireValue(ref(a))
          case LeftShift(a, count)    => (wireValue(ref(a))) << count
          case RightShift(a, count)   => (wireValue(ref(a))) >> count
          case input                  => throw new Exception(s"unknown input '${input}'")
        }
        cache(operation -> retval)
        retval
      }
    }

    input.map{case (wire, op) => (wire -> wireValue(op))}
      .toMap
  }
}

object Day7Solution {
  def SolveSignalInWireA(): UShort = {
    val input = ParseInput()
    val circuit = LogicGateEmulator.process(input)
    circuit("a")
  }

  def ParseInput(): LogicGateEmulator.UnsolvedCircuit = {
    val input = GetInput()
    input.map(Circuit.parse)
  }

  private def GetInput(): List[String] = {
    InputReader.ReadInput("/Day7.txt").split("\n").toList
  }
}
