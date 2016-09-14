import org.scalatest.FunSuite

class Day16Tests extends FunSuite {
  test("parse some input") {
    val result = SueParser.parse("Sue 1: cars: 9, akitas: 3, goldfish: 0")

    assert(result == (1 -> Map("cars" -> 9,
                               "akitas" -> 3,
                               "goldfish" -> 0)))
  }
}

class SueMatcherTests extends FunSuite {
  test("match intricately") {
    val sues = List("Sue 1: cats: 9, akitas: 3, goldfish: 3").map(SueParser.parse).toMap

    assert(SueMatcher.matchIntricately(sues, ("cats", 5)).size == 1)
    assert(SueMatcher.matchIntricately(sues, ("goldfish", 4)).size == 1)

    assert(SueMatcher.matchIntricately(sues, ("akitas", 3)).size == 1)
  }
}

class Day16SolutionTests extends BaseSolutionTests {
  test("can parse all input") {
    dontRunSolutionAutomatically {
      val input = Day16Solution.readInput()
      val sues = Day16Solution.parseInput(input)
      assert(sues.size == input.size)
    }
  }

  test("solve: find the aunt Sue with these specific facts") {
    dontRunSolutionAutomatically {
      val sue = Day16Solution.solvePart1(facts)
      // sue: Map[Int,Map[String,Int]] = Map(373 -> Map(pomeranians -> 3, perfumes -> 1, vizslas -> 0))
    }
  }

  test("solve part 2") {
    dontRunSolutionAutomatically {
      val sue = Day16Solution.solvePart2(facts)
      // sue: Day16Solution.Sues = Map(260 -> Map(goldfish -> 0, vizslas -> 0, samoyeds -> 2))
    }
  }

  val facts = SueParser.parseFacts("children: 3",
                                   "cats: 7",
                                   "samoyeds: 2",
                                   "pomeranians: 3",
                                   "akitas: 0",
                                   "vizslas: 0",
                                   "goldfish: 5",
                                   "trees: 3",
                                   "cars: 2",
                                   "perfumes: 1")

}
