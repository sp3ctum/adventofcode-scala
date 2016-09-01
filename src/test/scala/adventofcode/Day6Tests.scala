import org.scalatest.FunSuite

class Day6LightTests extends FunSuite {
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
}

class Day6BooleanLightManipulatorTests extends FunSuite {
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

  val emptyGrid = BooleanLightGridManipulator.initialGrid

  def process(rawInstructions: String*): BooleanLightGridManipulator.LightGrid = {
    val instructions = rawInstructions.map(LightInstruction.parse).toArray
    BooleanLightGridManipulator.process(instructions)
  }
}

class Day6BrightnessLightManipulatorTests extends FunSuite {
  test("turn lights on once") {
    val grid = process("turn on 1,1 through 1,1")
    assert(grid(LightCoordinate(1,1)) == 1)
  }

  test("turn lights on twice") {
    val grid = process("turn on 1,1 through 1,1",
                       "turn on 1,1 through 1,1")
    assert(grid(LightCoordinate(1,1)) == 2)
  }

  test("turning lights off should limit brightness at 0") {
    val grid = process("turn off 1,1 through 1,1")
    assert(grid(LightCoordinate(1,1)) == 0)
  }

  test("toggling light will increase brightness by 2") {
    val grid = process("toggle 1,1 through 1,1")
    assert(grid(LightCoordinate(1,1)) == 2)
  }

  val emptyGrid = BooleanLightGridManipulator.initialGrid

  def process(rawInstructions: String*): BrightnessLightGridManipulator.LightGrid = {
    val instructions = rawInstructions.map(LightInstruction.parse).toArray
    BrightnessLightGridManipulator.process(instructions)
  }
}

class Day6SolutionTests extends BaseSolutionTests {
  test("how many lights are on after all instructions?") {
    dontRunSolutionAutomatically {
      Day6Solution.HowManyLightsAreLitAfterInstructions()
      // res2: Int = 543903
      // Took about 20 seconds to calculate though
    }
  }

  test("how many lights are on after second set of instructions?") {
    dontRunSolutionAutomatically {
      Day6Solution.AmountOfLightBrightnessAfterSecondSetofInstructions()
      // res4: Int = 14687245
      // Runinng this took about a little longer.
    }
  }
}
