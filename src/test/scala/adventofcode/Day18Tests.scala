import org.scalatest.{FunSpec, FunSuite, WordSpec}

class Day18Tests extends FunSuite {
  test("get a light's neighbors") {
    val grid = parse(
      ".#....",
      "##....",
      "......",
      "......",
      "....##",
      "....#."
    )

    assert(GameOfLight.getNeighbors(0, 0, grid) == List(true, true, true))
    assert(GameOfLight.getNeighbors(5, 5, grid) == List(true, true, true))

    val grid2 = parse(
      "......",
      "..###.",
      "..#.#.",
      "..###.",
      "......",
      "......"
    )

    assert(GameOfLight.getNeighbors(3, 2, grid2) == List.fill(8)(true))
  }

  test("advance the state of lights according to given example (part 1)") {
    assert(nextState(ExampleLightStates.initial) == ExampleLightStates.step1)
    assert(nextState(ExampleLightStates.step1) == ExampleLightStates.step2)
    assert(nextState(ExampleLightStates.step2) == ExampleLightStates.step3)
    assert(nextState(ExampleLightStates.step3) == ExampleLightStates.step4)
  }

  test("advance the state of lights according to given example (part 2)") {
    def next = nextStateWithoutTouchingCorners _

    assert(next(SecondExampleLightStates.initial) == SecondExampleLightStates.step1)
    // assert(next(SecondExampleLightStates.step1) == SecondExampleLightStates.step2)
    // assert(next(SecondExampleLightStates.step2) == SecondExampleLightStates.step3)
    // assert(next(SecondExampleLightStates.step3) == SecondExampleLightStates.step4)
  }

  def parse(lines: String*): GameOfLight.Grid = {
    GameOfLight.parse(lines.toArray)
  }

  def serialize(grid: GameOfLight.Grid) = {
    grid.map(line => line.map(light =>
      if (light == true) '#'
      else '.').mkString).toList
  }

  def print(grid: GameOfLight.Grid): Unit = {
    val serialized = serialize(grid)
    serialized.foreach(println)
  }

  def nextState(grid: List[String]) = {
    val parsed = GameOfLight.parse(grid.toArray)
    val result = GameOfLight.getNextState(parsed)
    serialize(result)
  }

  def nextStateWithoutTouchingCorners(grid: List[String]) = {
    val parsed = GameOfLight.parse(grid.toArray)
    val result = GameOfLight.getNextStateWithStuckCorners(parsed)
    serialize(result)
  }
}

object ExampleLightStates {
  // these examples are the correct examples from the instruction page at
  // http://adventofcode.com/day/18
  val initial = List(
    ".#.#.#",
    "...##.",
    "#....#",
    "..#...",
    "#.#..#",
    "####.."
  )
  val step1 = List(
    "..##..",
    "..##.#",
    "...##.",
    "......",
    "#.....",
    "#.##.."
  )
  val step2 = List(
    "..###.",
    "......",
    "..###.",
    "......",
    ".#....",
    ".#...."
  )
  val step3 = List(
    "...#..",
    "......",
    "...#..",
    "..##..",
    "......",
    "......"
  )
  val step4 = List(
    "......",
    "......",
    "..##..",
    "..##..",
    "......",
    "......"
  )
}

object SecondExampleLightStates {
  val initial = List(
    "##.#.#",
    "...##.",
    "#....#",
    "..#...",
    "#.#..#",
    "####.#"
  )

  val step1 = List(
    "#.##.#",
    "####.#",
    "...##.",
    "......",
    "#...#.",
    "#.####"
  )

  val step2 = List(
    "#..#.#",
    "#....#",
    ".#.##.",
    "...##.",
    ".#..##",
    "##.###"
  )

  val step3 = List(
    "#...##",
    "####.#",
    "..##.#",
    "......",
    "##....",
    "####.#"
  )

  val step4 = List(
    "#.####",
    "#....#",
    "...#..",
    ".##...",
    "#.....",
    "#.#..#"
  )

  val step5 = List(
    "##.###",
    ".##..#",
    ".##...",
    ".##...",
    "#.#...",
    "##...#"
  )
}

class Day18SolutionTests extends BaseSolutionTests {
  test("can parse input") {
    dontRunSolutionAutomatically {
      val input = Day18Solution.readInput()
      Day18Solution.parseInput(input)
    }
  }

  test("solve part1") {
    dontRunSolutionAutomatically {
      Day18Solution.solveCountOfLightsOn()
      // res108: Int = 1061
      // took about 1-2 seconds
    }
  }

  test("solve part2") {
    dontRunSolutionAutomatically {
      Day18Solution.solveCountOfLightsOnPart2()
      // res114: Int = 1006
    }
  }
}
