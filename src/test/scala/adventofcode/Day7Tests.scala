import org.scalatest.FunSuite
import spire.math.UShort

class Day7LogicTests extends FunSuite {
  import scala.language.implicitConversions

  test("literal value") {
    val result = process(List("aaaaa" -> (LiteralValue(3))))
    assert(result == circuit("aaaaa" -> 3))
  }

  test("direct connection") {
    val result = process(List("a" -> (LiteralValue(1)),
                              "b" -> (DirectConnection("a"))))
  }

  test("AND wires") {
    val result = process(List("a" -> (LiteralValue(1)),
                              "b" -> (LiteralValue(2)),
                              "c" -> (AndOperation(WireRef("a"), WireRef("b")))))

    assert(result == circuit("a" -> 1,
                             "b" -> 2,
                             "c" -> 0))
  }

  test("OR wires") {
    val result = process(List("a" -> (LiteralValue(1)),
                              "b" -> (LiteralValue(2)),
                              "c" -> (OrWires("a", "b"))))

    assert(result == circuit("a" -> 1,
                             "b" -> 2,
                             "c" -> 3))
  }

  test("NOT wire") {
    val result = process(List("a" -> (LiteralValue(8)),
                              "b" -> (NotWire("a"))))
    assert(result == circuit("a" -> 8,
                             "b" -> 65527))
  }

  test("LSHIFT a wire") {
    val result = process(List("a" -> (LiteralValue(1)),
                              "b" -> (LeftShift("a", 1))))
    assert(result == circuit("a" -> 1,
                             "b" -> 2))
  }

  test("RSHIFT a wire") {
    val result = process(List("a" -> (LiteralValue(1)),
                              "b" -> (RightShift("a", 1))))
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
1 AND cx -> cy
NOT x -> h
h -> i""".split("\n").filterNot{_ == ""}.map(Circuit.parse)

    val expected = List("x" -> LiteralValue(123),
                        "d" -> AndOperation(WireRef("x"),WireRef("y")),
                        "e" -> OrWires("x","y"),
                        "f" -> LeftShift("x",2),
                        "g" -> RightShift("y",2),
                        "cy" -> AndOperation(NumberRef(1),WireRef("cx")),
                        "h" -> NotWire("x"),
                        "i" -> DirectConnection("h"))

    for (i <- instructions.indices) {
      assert(instructions(i) == expected(i))
    }
  }

  test("parse and solve instructions") {
    val instructions = """
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
i -> j""".split("\n").filterNot{_ == ""}.map(Circuit.parse).toList

    val result = process(instructions)
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

  test("instructions are calculated lazily") {
    val result = process(List("c" -> OrWires("a", "b"),
                              "a" -> LiteralValue(1),
                              "b" -> LiteralValue(2)))

    assert(result == circuit("a" -> 1,
                             "b" -> 2,
                             "c" -> 3))
  }

  val process = LogicGateEmulator.process _

  def circuit(values: (String, UShort)*): LogicGateEmulator.SolvedCircuit = {
    values.toMap
  }

  // trying these out
  implicit def intToUShort(a: Int): UShort = UShort(a)
}

class Day7SolutionTests extends BaseSolutionTests {
  test("parse all data (exploratory test)") {
    Day7Solution.ParseInput() // should not crash
  }

  test("solve first circuit") {
    dontRunSolutionAutomatically {
      val result = Day7Solution.SolveSignalInWireA()
    }
  }
}
