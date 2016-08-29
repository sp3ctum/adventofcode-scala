import org.scalatest.{ Canceled, FunSuite, Ignore }

class BaseSolutionTests extends FunSuite {
  def dontRunSolutionAutomatically(block: => Unit): Unit = {
    // things marked with this should be run in the repl or with a main method
    // somewhere
    cancel("not running solution automatically")
  }
}

class DirectionsTests extends FunSuite {
  test("parsing both inputs") {
    assert(Directions.Parse("()") == List(Up(), Down()))
  }

  test("calculating destination floor") {
    assert(Directions.CalculateFloor(List(Up(), Down())) == 0)
  }
}

class SolutionTests extends BaseSolutionTests {
  test("solution for Day1") {
    dontRunSolutionAutomatically {
      Solution.Calculate()
    }
  }
}
