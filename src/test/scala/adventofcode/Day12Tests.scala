import org.scalatest.FunSuite
import org.json4s.JsonAST._
import org.json4s.jackson.JsonMethods

class Day12Tests extends FunSuite {
  test("non-red objects") {
    // does not ignore arrays
    assert(SantaJsonFilter.withoutNonRedObjects(parse("[1,2,3]")) == parse("[1,2,3]"))

    // ignores red objects
    assert(SantaJsonFilter.withoutNonRedObjects(parse("""[1,{"c":"red","b":2},3]""")) == parse("[1,3]"))

    // does not ignore non-red objects
    assert(SantaJsonFilter.withoutNonRedObjects(parse("""[1,"red",5]""")) == parse("""[1,"red",5]"""))
  }

  def parse(json: String): JValue = JsonMethods.parse(json)
}

class Day12SolutionTests extends BaseSolutionTests {
  test("solve part 1") {
    dontRunSolutionAutomatically {
      Day12Solution.solveSum()
      // BigInt = 191164
    }
  }

  test("solve part 2") {
    dontRunSolutionAutomatically {
      Day12Solution.solveNonRedSum()
      // BigInt = 87842
    }
  }
}
