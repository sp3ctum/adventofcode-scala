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

  test("is a coordinate contained in a range") {
    val range = LightCoordinateRange(LightCoordinate(1,1), LightCoordinate(2,2))
    assert(range.contains(LightCoordinate(1,1)))
    assert(range.contains(LightCoordinate(1,2)))
    assert(range.contains(LightCoordinate(2,1)))
    assert(range.contains(LightCoordinate(2,2)))

    assert(! range.contains(LightCoordinate(3,2)))
    assert(! range.contains(LightCoordinate(2,3)))
    assert(! range.contains(LightCoordinate(3,3)))
  }

  test("turn lights on") {
    val grid = process("turn on 1,1 through 2,2")

    assert(grid(LightCoordinate(1,1)) == true)
    assert(grid(LightCoordinate(2,1)) == true)
    assert(grid(LightCoordinate(1,2)) == true)
    assert(grid(LightCoordinate(2,2)) == true)

    // off limits should be untouched
    assert(grid(LightCoordinate(3,2)) == false)
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
    }
  }
}
