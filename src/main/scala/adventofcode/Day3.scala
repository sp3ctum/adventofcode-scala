// --- Day 3: Perfectly Spherical Houses in a Vacuum ---
//
// Santa is delivering presents to an infinite two-dimensional grid of houses.
//
// He begins by delivering a present to the house at his starting location, and
// then an elf at the North Pole calls him via radio and tells him where to move
// next. Moves are always exactly one house to the north (^), south (v), east
// (>), or west (<). After each move, he delivers another present to the house
// at his new location.
//
// However, the elf back at the north pole has had a little too much eggnog, and
// so his directions are a little off, and Santa ends up visiting some houses
// more than once. How many houses receive at least one present?
//
// For example:
//
// > delivers presents to 2 houses: one at the starting location, and one to the
// east.
//
// ^>v< delivers presents to 4 houses in a square, including twice to the house
// at his starting/ending location.
//
// ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2
// houses.

abstract sealed class SantaDirection
case class North() extends SantaDirection
case class East() extends SantaDirection
case class South() extends SantaDirection
case class West() extends SantaDirection

object SantaDirection {
  def Parse(dir: Char): SantaDirection = {
    dir match {
      case '^' => North()
      case '>' => East()
      case 'v' => South()
      case '<' => West()
    }
  }
}

case class SantaPosition(x: Int, y: Int)
object SantaPosition {
  def Start = SantaPosition(0, 0)
}


object TravelLog {
  def HousesVisitedAtLeastOnce(route: List[SantaDirection]): Set[SantaPosition] = {
    val houses = VisitedHouses(route)
    houses.toSet
  }

  def AmountOfHousesVisitedAtLeastOnce(route: List[SantaDirection]): Int =
    HousesVisitedAtLeastOnce(route).size

  // note: santa will start his journey from the origin (0,0) point
  // the X axis increases to the right, and the Y axis upward
  def VisitedHouses(route: List[SantaDirection]): List[SantaPosition] = {
    val start = List(SantaPosition(0, 0))
    route.foldLeft(start)((result, newDirection) => {
                            val newHousePosition = MoveFrom(result.head, newDirection)
                            newHousePosition :: result
                          }
    ).reverse
  }

  def MoveFrom(start: SantaPosition, to: SantaDirection): SantaPosition = {
    to match {
      case North() => start.copy(y = start.y + 1)
      case East() => start.copy(x = start.x + 1)
      case South() => start.copy(y = start.y - 1)
      case West() => start.copy(x = start.x - 1)
    }
  }
}

object Day3Solution {
  def HousesVisitedAtLeastOnce(): Int = {
    val route = GetDirections()
    TravelLog.HousesVisitedAtLeastOnce(route).size
  }

  def GetDirections(): List[SantaDirection] = {
    val input = ReadInput()
    input.map(SantaDirection.Parse).toList
  }

  private def ReadInput(): String = InputReader.ReadInput("/Day3.txt")

  // --- Part Two ---
  //
  // The next year, to speed up the process, Santa creates a robot version of
  // himself, Robo-Santa, to deliver presents with him.
  //
  // Santa and Robo-Santa start at the same location (delivering two presents to
  // the same starting house), then take turns moving based on instructions from
  // the elf, who is eggnoggedly reading from the same script as the previous
  // year.
  //
  // This year, how many houses receive at least one present?
  //
  // For example:
  //
  // ^v delivers presents to 3 houses, because Santa goes north, and then
  // Robo-Santa goes south.
  //
  // ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up
  // back where they started.
  //
  // ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one
  // direction and Robo-Santa going the other.

  def HousesVisitedBySantaAndRoboSanta(): Int = {
    // because we aren't interested in who walks what route, we can calculate
    // them together
    val routes = GetDirections().zipWithIndex

    val santaRoutes = routes.collect({case (d, index) if index % 2 == 0 => d})
    val roboRoutes = routes.collect({case (d, index) if index % 2 != 0 => d})

    val santaHouses = TravelLog.HousesVisitedAtLeastOnce(santaRoutes)
    val roboHouses = TravelLog.HousesVisitedAtLeastOnce(roboRoutes)

    santaHouses.union(roboHouses).size
  }

}
