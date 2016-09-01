import org.scalatest.FunSuite

class Day6Tests extends FunSuite {
  test("parsing instructions") {
    assert(LightInstruction.parse("toggle 461,550 through 564,900") ==
             LightsToggle(LightCoordinateRange(LightCoordinate(461,550),
                                               LightCoordinate(564,900))))

    assert(LightInstruction.parse("turn on 461,550 through 564,900") ==
             LightsOn(LightCoordinateRange(LightCoordinate(461,550),
                                           LightCoordinate(564,900))))

    assert(LightInstruction.parse("turn off 461,550 through 564,900") ==
             LightsOff(LightCoordinateRange(LightCoordinate(461,550),
                                            LightCoordinate(564,900))))
  }

  test("get coordinates in a range") {
    val range = LightCoordinateRange(LightCoordinate(1,1),
                                     LightCoordinate(3,1))

    val coordinates = range.elements()

    assert(coordinates == List(LightCoordinate(1,1),
                               LightCoordinate(2,1),
                               LightCoordinate(3,1)))
  }

  test("turn lights on") {
    val grid = process("turn on 1,1 through 2,2")

    assert(grid(LightCoordinate(1,1)) == true)
    assert(grid(LightCoordinate(2,1)) == true)
    assert(grid(LightCoordinate(1,2)) == true)
    assert(grid(LightCoordinate(2,2)) == true)

    // off limits should be untouched
    assert(grid.getOrElse(LightCoordinate(3,2), false) == false)
  }

  test("turn lights off") {
    val grid = process("turn on 1,1 through 2,2",
                       "turn off 2,2 through 2,2")

    assert(grid(LightCoordinate(1,1)) == true)
    assert(grid(LightCoordinate(2,1)) == true)
    assert(grid(LightCoordinate(1,2)) == true)
    assert(grid(LightCoordinate(2,2)) == false)
  }

  val emptyGrid = LightGridManipulator.initialGrid

  def process(rawInstructions: String*): LightGridManipulator.LightGrid = {
    val instructions = rawInstructions.map(LightInstruction.parse).toArray
    LightGridManipulator.process(instructions)
  }
}

class Day6SolutionTests extends BaseSolutionTests {
  test("how many lights are on after all instructions?") {
    dontRunSolutionAutomatically {
      Day6Solution.HowManyLightsAreLitAfterInstructions()
      // res2: Int = 543903
      // took about 20 seconds to calculate though
    }
  }
}
