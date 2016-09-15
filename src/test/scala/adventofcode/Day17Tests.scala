import org.scalatest.FunSuite

class Day17Tests extends FunSuite {
  test("get combinations of containers") {
    val containers = List(20, 15, 10, 5, 5)
    val combinations = Day17Solution.combinationsOfContainers(containers, 25)
    assert(combinations.length == 4)
  }

  test("get minimum number of containers") {
    val containers = List(20, 15, 10, 5, 5)
    val combinations = Day17Solution.combinationsOfContainers(containers, 25).toList

    val smallestNumberOfContainers = combinations
      .groupBy(_.length)
      .minBy { case (size, _) => size }
  }
}

class Day17SolutionTests extends BaseSolutionTests {
  test("solve part1 by brute force") {
    dontRunSolutionAutomatically {
      Day17Solution.solve()
      // res61: Int = 654
    }
  }

  test("solve part2 by brute force also") {
    dontRunSolutionAutomatically {
      val (numberOfElements, combinations) = Day17Solution.solvePart2()
      combinations.size
      // res97: Int = 57
    }
  }
}
