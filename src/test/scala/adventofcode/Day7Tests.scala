import org.scalatest.FunSuite
import spire.math.UShort

class Day7LogicTests extends FunSuite {
  import scala.language.implicitConversions

  test("literal value") {
    val result = process((LiteralValue(3), "aaaaa"))
    assert(result == circuit("aaaaa" -> 3))
  }

  test("direct connection") {
    val result = process((LiteralValue(1), "a"),
                         (DirectConnection("a"), "b"))
  }

  test("AND wires") {
    val result = process((LiteralValue(1), "a"),
                         (LiteralValue(2), "b"),
                         (AndWires("a", "b"), "c"))

    assert(result == circuit("a" -> 1,
                             "b" -> 2,
                             "c" -> 0))
  }

  test("OR wires") {
    val result = process((LiteralValue(1), "a"),
                         (LiteralValue(2), "b"),
                         (OrWires("a", "b"), "c"))

    assert(result == circuit("a" -> 1,
                             "b" -> 2,
                             "c" -> 3))
  }

  test("NOT wire") {
    val result = process((LiteralValue(8), "a"),
                         (NotWire("a"), "b"))
    assert(result == circuit("a" -> 8,
                             "b" -> 65527))
  }

  test("LSHIFT a wire") {
    val result = process((LiteralValue(1), "a"),
                         (LeftShift("a", 1), "b"))
    assert(result == circuit("a" -> 1,
                             "b" -> 2))
  }

  test("RSHIFT a wire") {
    val result = process((LiteralValue(1), "a"),
                         (RightShift("a", 1), "b"))
    assert(result == circuit("a" -> 1,
                             "b" -> 0))
  }

  test("parse instructions") {
    val instructions = """
123 -> x
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
h -> i""" split("\n") filterNot{_ == ""} map(Instruction.parse)

    val expected = Array(Instruction(LiteralValue(123),"x"),
                         Instruction(AndWires("x","y"),"d"),
                         Instruction(OrWires("x","y"),"e"),
                         Instruction(LeftShift("x",2),"f"),
                         Instruction(RightShift("y",2),"g"),
                         Instruction(NotWire("x"),"h"),
                         Instruction(DirectConnection("h"),"i"))

    for (i <- instructions.indices) {
      assert(instructions(i) == expected(i))
    }
  }

  test("") {
    val instructions = """
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
i -> j""" split("\n") filterNot{_ == ""} map(Instruction.parse)

    val result = LogicGateEmulator.process(instructions)
    val expected = circuit("d" -> 72,
                           "e" -> 507,
                           "f" -> 492,
                           "g" -> 114,
                           "h" -> 65412,
                           "i" -> 65079,
                           "j" -> 65079,
                           "x" -> 123,
                           "y" -> 456)
    assert(result == expected)
  }

  def process(operations: (Operation,String)*): LogicGateEmulator.Circuit = {
    val instructions = operations.map{case (op,wire) => Instruction(op, wire)}
    LogicGateEmulator.process(instructions.toArray)
  }

  def circuit(values: (String, UShort)*): LogicGateEmulator.Circuit = {
    values.toMap
  }

  // trying these out
  implicit def intToUShort(a: Int): UShort = UShort(a)
}

class Day7SolutionTests extends BaseSolutionTests {
  test("solve first circuit") {
    dontRunSolutionAutomatically {
      val result = Day7Solution.SolveSignalInWireA()
    }
  }
}
