import org.scalatest.FunSuite

class Day16Tests extends FunSuite {
  test("parse some input") {
    val input = "Sue 1: cars: 9, akitas: 3, goldfish: 0"
    val result = SueParser.parse(input)

    assert(result == (1 -> Map("cars" -> 9,
                               "akitas" -> 3,
                               "goldfish" -> 0)))
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

      val sue = Day16Solution.findSue(facts)
      // sue: Map[Int,Map[String,Int]] = Map(373 -> Map(pomeranians -> 3, perfumes -> 1, vizslas -> 0))
    }
  }
}
