import org.scalatest.FunSuite
import scala.collection.immutable.Stream

class ReindeerDistanceMeasurerTests extends FunSuite {
  test("calculate reindeer race") {
    val r = Reindeer("Rudolph", Flying(10, time = 2), restTime = 1)
    assert(ReindeerDistanceMeasurer.reindeerRace(r).take(11).toList
             ==
             List(0, 10, 20, 20, 30, 40, 40, 50, 60, 60, 70))
  }

  test("get distance at n seconds") {
    val comet = Reindeer("Comet", Flying(14, 10), 127)
    val dancer = Reindeer("Dancer", Flying(16, 11), 162)

    assert(ReindeerDistanceMeasurer.distanceAtSeconds(1, comet) == 14)
    assert(ReindeerDistanceMeasurer.distanceAtSeconds(1, dancer) == 16)

    assert(ReindeerDistanceMeasurer.distanceAtSeconds(11, comet) == 140)
    assert(ReindeerDistanceMeasurer.distanceAtSeconds(11, dancer) == 176)

    assert(ReindeerDistanceMeasurer.distanceAtSeconds(1000, comet) == 1120)
    assert(ReindeerDistanceMeasurer.distanceAtSeconds(1000, dancer) == 1056)
  }
}

class Day14SolutionTests extends BaseSolutionTests {
  test("parse input") {
    dontRunSolutionAutomatically {
      Day14Solution.parseInput()
    }
  }

  test("solve part 1") {
    dontRunSolutionAutomatically {
      Day14Solution.solve()
      // res68: (Reindeer, Int) = (Reindeer(Vixen,Flying(19,7),124),2660)
    }
  }
}
