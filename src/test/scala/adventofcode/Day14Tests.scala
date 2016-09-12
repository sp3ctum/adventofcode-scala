import org.scalatest.FunSuite

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

class ReindeerPointTests extends FunSuite {
  test("give points for current leader") {
    val comet = Reindeer("Comet", Flying(0, 1), 2)
    val dancer = Reindeer("Dancer", Flying(1, 1), 2)

    val points = ReindeerPointCalculator.getRaceStatusAtSeconds(List(comet, dancer), 1)
    assert(points == Map(comet -> 0, dancer -> 1))
  }

  test("give points to multiple tied leaders") {
    // these reindeers will always be tied
    val adolph = Reindeer("Adolph", Flying(10, 1), 1)
    val berta = Reindeer("Berta", Flying(10, 1), 1)

    val points = ReindeerPointCalculator.getRaceStatusAtSeconds(List(adolph, berta), 1)
    assert(points == Map(adolph -> 1, berta -> 1))
  }

  test("points for reindeers at n seconds (copied example)") {
    val comet = Reindeer("Comet", Flying(14, 10), 127)
    val dancer = Reindeer("Dancer", Flying(16, 11), 162)

    val raceStatus = ReindeerPointCalculator.getRaceStatusAtSeconds(List(comet, dancer), 1000)

    assert(raceStatus == Map(dancer -> 689,
                             comet -> 312))
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

  test("solve part 2") {
    dontRunSolutionAutomatically {
      Day14Solution.solvePart2()
      // res9: (Reindeer, Int) = (Reindeer(Blitzen,Flying(19,9),158),1256)
    }
  }
}
