import org.scalatest.FunSuite

class Day10Tests extends FunSuite {
  test("encoding") {
    assert(LookAndSay.encode("1") == "11")
    assert(LookAndSay.encode("11") == "21")
    assert(LookAndSay.encode("21") == "1211")
    assert(LookAndSay.encode("1211") == "111221")
    assert(LookAndSay.encode("111221") == "312211")
  }

  test("encode repeatedly") {
    assert(LookAndSay.encodeTimes("1", 5) == "312211")
  }
}

class Day10SolutionTests extends BaseSolutionTests {
  test("solve part 1") {
    dontRunSolutionAutomatically {
      Day10Solution.solve().length()
      // res5: Int = 252594
      // this takes about 0.2 seconds
    }
  }

  test("solve part 2") {
    dontRunSolutionAutomatically {
      Day10Solution.solvePart2().length()
      // res5: Int = 3579328
      // this took about 8 seconds
    }
  }
}
