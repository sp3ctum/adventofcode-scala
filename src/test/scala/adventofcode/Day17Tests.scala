import org.scalatest.FunSuite

class Day17Tests extends FunSuite {
  test("get combinations of containers") {
    val containers = List(20, 15, 10, 5, 5)
    val combinations = Day17Solution.combinationsOfContainers(containers, 25)
    assert(combinations.length == 4)
  }
}

class Day17SolutionTests extends BaseSolutionTests {
  test("solve part1 by brute force") {
    dontRunSolutionAutomatically {
      Day17Solution.solve()
      // res61: Int = 654
    }
  }
}
