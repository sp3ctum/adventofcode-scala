// --- Day 6: Probably a Fire Hazard ---
//
// Because your neighbors keep defeating you in the holiday house decorating
// contest year after year, you've decided to deploy one million lights in a
// 1000x1000 grid.
//
// Furthermore, because you've been especially nice this year, Santa has mailed
// you instructions on how to display the ideal lighting configuration.
//
// Lights in your grid are numbered from 0 to 999 in each direction; the lights
// at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions
// include whether to turn on, turn off, or toggle various inclusive ranges
// given as coordinate pairs. Each coordinate pair represents opposite corners
// of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore
// refers to 9 lights in a 3x3 square. The lights all start turned off.
//
// To defeat your neighbors this year, all you have to do is set up your lights
// by doing the instructions Santa sent you in order.
//
// For example:
//
// - turn on 0,0 through 999,999 would turn on (or leave on) every light.
// - toggle 0,0 through 999,0 would toggle the first line of 1000 lights,
// turning off the ones that were on, and turning on the ones that were off.
// - turn off 499,499 through 500,500 would turn off (or leave off) the middle
// four lights.
//
// After following the instructions, how many lights are lit?

case class LightCoordinate(x: Int, y: Int)

case class LightCoordinateRange(start: LightCoordinate, end: LightCoordinate) {
  def elements(): List[LightCoordinate] = {
    for (y <- (start.y to end.y).toList;
         x <- (start.x to end.x).toList) yield {
      LightCoordinate(x, y)
    }
  }
}

object LightCoordinateRange {
  def create(xa: String, ya: String, xb: String, yb: String): LightCoordinateRange = {
    LightCoordinateRange(LightCoordinate(xa.toInt, ya.toInt),
                        LightCoordinate(xb.toInt, yb.toInt))
  }
}

abstract sealed class LightInstruction
case class LightsOn(location: LightCoordinateRange) extends LightInstruction
case class LightsOff(location: LightCoordinateRange) extends LightInstruction
case class LightsToggle(location: LightCoordinateRange) extends LightInstruction

object LightInstruction {
  def parse(input: String): LightInstruction = {
    val position = "(\\d+),(\\d+)"
    val turnOn  = s"^turn on ${position} through ${position}".r
    val turnOff = s"^turn off ${position} through ${position}".r
    val toggle  = s"^toggle ${position} through ${position}".r
    val coords = LightCoordinateRange.create _

    input match {
      case turnOn (xa, ya, xb, yb) => LightsOn(coords(xa, ya, xb, yb))
      case turnOff(xa, ya, xb, yb) => LightsOff(coords(xa, ya, xb, yb))
      case toggle (xa, ya, xb, yb) => LightsToggle(coords(xa, ya, xb, yb))
    }
  }
}

object LightGridManipulator {
  type LightGrid = Map[LightCoordinate,Boolean]

  def process(instructions: Array[LightInstruction]): LightGrid = {
    instructions.foldLeft(initialGrid)(process)
  }

  private def process(grid: LightGrid, instruction: LightInstruction): LightGrid = {
    instruction match {
      case LightsOn(location) => set(light => true, location, grid)
      case LightsOff(location) => set(light => false, location, grid)
      case LightsToggle(location) => set(light => ! light, location, grid)
    }
  }

  private def set(action: Boolean => Boolean,
                  targetLocation: LightCoordinateRange,
                  grid: LightGrid): LightGrid = {
    val changedLights = targetLocation.elements.map{
      case coord @ LightCoordinate(x,y) => {
        val light = grid.getOrElse(LightCoordinate(x,y), false)
        (coord, action(light))
      }
    }
    // overwrite the old lights with changedLights
    grid ++ changedLights
  }

  val initialGrid: LightGrid = Map()

}

object Day6Solution {
  def HowManyLightsAreLitAfterInstructions(): Int = {
    val instructions = parseInput()
    val resultGrid = LightGridManipulator.process(instructions)
    resultGrid.values.count(onStatus => onStatus == true)
  }

  private def parseInput(): Array[LightInstruction] = {
    val input = getInput()
    input.map(LightInstruction.parse)
  }

  private def getInput(): Array[String] = {
    InputReader.ReadInput("/Day6.txt").split("\n")
  }
}
