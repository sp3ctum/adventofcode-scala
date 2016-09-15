import java.lang.Math

// --- Day 18: Like a GIF For Your Yard ---
//
// After the million lights incident, the fire code has gotten stricter: now, at
// most ten thousand lights are allowed. You arrange them in a 100x100 grid.
//
// Never one to let you down, Santa again mails you instructions on the ideal
// lighting configuration. With so few lights, he says, you'll have to resort to
// animation.
//
// Start by setting your lights to the included initial configuration (your
// puzzle input). A # means "on", and a . means "off".
//
// Then, animate your grid in steps, where each step decides the next
// configuration based on the current one. Each light's next state (either on or
// off) depends on its current state and the current states of the eight lights
// adjacent to it (including diagonals). Lights on the edge of the grid might
// have fewer than eight neighborsOn; the missing ones always count as "off".
//
// For example, in a simplified 6x6 grid, the light marked A has the neighborsOn
// numbered 1 through 8, and the light marked B, which is on an edge, only has
// the neighborsOn marked 1 through 5:
//
// 1B5...
// 234...
// ......
// ..123.
// ..8A4.
// ..765.
//
// The state a light should have next is based on its current state (on or off)
// plus the number of neighborsOn that are on:
//
// - A light which is on stays on when 2 or 3 neighborsOn are on, and turns off
// otherwise.
// - A light which is off turns on if exactly 3 neighborsOn are on, and stays off
// otherwise.
// - All of the lights update simultaneously; they all consider the same current
// state before moving to the next.
//
// Here's a few steps from an example configuration of another 6x6 grid:
//
// Initial state:
// .#.#.#
// ...##.
// #....#
// ..#...
// #.#..#
// ####..
//
// After 1 step:
// ..##..
// ..##.#
// ...##.
// ......
// #.....
// #.##..
//
// After 2 steps:
// ..###.
// ......
// ..###.
// ......
// .#....
// .#....
//
// After 3 steps:
// ...#..
// ......
// ...#..
// ..##..
// ......
// ......
//
// After 4 steps:
// ......
// ......
// ..##..
// ..##..
// ......
// ......
//
// After 4 steps, this example has four lights on.
//
// In your grid of 100x100 lights, given your initial configuration, how many
// lights are on after 100 steps?

// Like the game of life
// -yeah, I'm so clever!
object GameOfLight {
  type Grid = Array[Array[Boolean]]

  def parse(lines: Array[String]): Grid = {
    lines.map(line => {
      line.map {
        case '.' => false
        case '#' => true
      }.toArray
    })
  }

  def getNextState(grid: Grid): Grid = {
    grid.zipWithIndex.par.map {
      case (line, y) => {
        line.zipWithIndex.map {
          case (light, x) => {
            switchLight(light, x, y, grid)
          }
        }
      }
    }.toArray
  }

  def switchLight(light: Boolean, x: Int, y: Int, grid: Grid): Boolean = {
    val neighborsOn = getNeighbors(x, y, grid).count(_ == true)
    if (light == true)
      neighborsOn == 2 || neighborsOn == 3
    else
      neighborsOn == 3
  }

  def getNeighbors(x: Int, y: Int, grid: Grid): List[Boolean] = {
    val columns = List(Math.max(x - 1, 0), x, Math.min(x + 1, grid.head.length - 1)).distinct
    val lines = List(Math.max(y - 1, 0), y, Math.min(y + 1, grid.length - 1)).distinct

    for {
      column <- columns
      line <- lines
      if !(column == x && line == y) // skip this light
    } yield grid(line)(column)

  }
}

object Day18Solution {
  def readInput(): Array[String] =
    InputReader.ReadInput("Day18.txt").split("\n")

  def parseInput(input: Array[String]): GameOfLight.Grid =
    GameOfLight.parse(input)

  def solveCountOfLightsOn(): Int = {
    val initialGrid = parseInput(readInput())
    val steps = 100

    val finalGrid = Iterator.iterate(initialGrid)(GameOfLight.getNextState)
      .take(steps + 1).toList.last

    finalGrid.map(line => line.count(_ == true)).sum
  }
}
