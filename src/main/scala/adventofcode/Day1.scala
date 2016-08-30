import scala.io.Source

// --- Day 1: Not Quite Lisp ---

// Santa was hoping for a white Christmas, but his weather machine's "snow"
// function is powered by stars, and he's fresh out! To save Christmas, he needs
// you to collect fifty stars by December 25th.

// Collect stars by helping Santa solve puzzles. Two puzzles will be made
// available on each day in the advent calendar; the second puzzle is unlocked
// when you complete the first. Each puzzle grants one star. Good luck!

// Here's an easy puzzle to warm you up.

// Santa is trying to deliver presents in a large apartment building, but he
// can't find the right floor - the directions he got are a little confusing. He
// starts on the ground floor (floor 0) and then follows the instructions one
// character at a time.

// An opening parenthesis, (, means he should go up one floor, and a closing
// parenthesis, ), means he should go down one floor.

// The apartment building is very tall, and the basement is very deep; he will
// never find the top or bottom floors.

// For example:

// (()) and ()() both result in floor 0.
// ((( and (()(()( both result in floor 3.
// ))((((( also results in floor 3.
// ()) and ))( both result in floor -1 (the first basement level).
// ))) and )())()) both result in floor -3.

// To what floor do the instructions take Santa?

abstract class Direction
case class Up() extends Direction
case class Down() extends Direction

case class SantasJourneyStatus(takenDirections: List[Direction] = List(),
                               currentFloor: Int = 0)

object Directions {
  def CalculateFloor(directions: List[Direction]): Int = {
    directions
      .map(DirectionToInt)
      .sum
  }

  private def DirectionToInt(d: Direction): Int = {
    d match {
      case Up() => 1
      case Down() => -1
    }
  }

  def CalculateDirectionEnteringLevel(directions: List[Direction], level: Int): Int = {
    val changes = FloorChanges(directions)
    changes.zipWithIndex.filter {
      case (status, index) => status.currentFloor == level
    }.head._2
  }

  private def FloorChanges(directions: List[Direction]): List[SantasJourneyStatus] = {
    directions.scanLeft(SantasJourneyStatus())(
      (result, newDirection) => {
        result.copy(takenDirections = result.takenDirections :+ newDirection,
                    currentFloor = result.currentFloor + DirectionToInt(newDirection))
      })
  }

  def Parse(input: String): List[Direction] = {
    input.map{_ match {
                case '(' => Up()
                case ')' => Down()
              }}
      .toList
  }
}

object Solution {
  def CalculateFinalFloor(): Int = {
    val input = ReadInput()
    val directions = Directions.Parse(input)
    Directions.CalculateFloor(directions)
  }

  def ReadInput(): String = {
    val url = getClass().getResource("/Day1.txt")
    val input = Source.fromURL(url)
    input.bufferedReader.readLine()
  }

  // --- Part Two ---
  //
  // Now, given the same instructions, find the position of the first character
  // that causes him to enter the basement (floor -1). The first character in the
  // instructions has position 1, the second character has position 2, and so on.
  //
  // For example:
  //
  // ) causes him to enter the basement at character position 1.
  // ()()) causes him to enter the basement at character position 5.
  //
  // What is the position of the character that causes Santa to first enter the
  // basement?
  //
  // personal note: the result should be 1-based
  def CalculateDirectionIndexThatEntersLevel(level: Int): Int = {
    val input = Solution.ReadInput()
    val directions = Directions.Parse(input)
    Directions.CalculateDirectionEnteringLevel(directions, level)
  }
}
