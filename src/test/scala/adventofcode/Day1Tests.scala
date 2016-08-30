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

  test("calculate direction entering specific level") {
    val toLevel = Directions.CalculateDirectionEnteringLevel _

    // These are from the example
    //
    // ) causes him to enter the basement at character position 1.
    // ()()) causes him to enter the basement at character position 5.
    assert(toLevel(Directions.Parse(")"), -1) == 1)
    assert(toLevel(Directions.Parse("()())"), -1) == 5)
  }
}

class SolutionTests extends BaseSolutionTests {
  test("solution for Day1") {
    dontRunSolutionAutomatically {
      Solution.CalculateFinalFloor()
    }
  }

  test("additional solution") {
    dontRunSolutionAutomatically {
      Solution.CalculateDirectionIndexThatEntersLevel(-1)
    }
  }
}
