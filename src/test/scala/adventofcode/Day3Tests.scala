import org.scalatest.FunSuite

class Day3Tests extends FunSuite {
  test("parsing input") {
    val input = "^>v<".map(SantaDirection.Parse).toList
    assert(input == List(North(), East(), South(), West()))
  }

  test("moving santa") {
    val start = SantaPosition.Start
    assert(TravelLog.MoveFrom(start, North()) == SantaPosition(0, 1))
    assert(TravelLog.MoveFrom(start, East()) == SantaPosition(1, 0))
    assert(TravelLog.MoveFrom(start, South()) == SantaPosition(0, -1))
    assert(TravelLog.MoveFrom(start, West()) == SantaPosition(-1, 0))
  }

  test("getting list of visited houses") {
    def takingRoute(r: String): List[SantaPosition] = {
      TravelLog.VisitedHouses(route(r))
    }

    assert(takingRoute(">") == List(SantaPosition(0, 0), SantaPosition(1, 0)))
    assert(takingRoute("^>v<") == List(SantaPosition(0,0),
                                       SantaPosition(0,1),
                                       SantaPosition(1,1),
                                       SantaPosition(1,0),
                                       SantaPosition(0,0)))
  }

  test("number of visited houses") {
    assert(TravelLog.AmountOfHousesVisitedAtLeastOnce(route(">")) == 2)
    assert(TravelLog.AmountOfHousesVisitedAtLeastOnce(route("^>v<")) == 4)
    assert(TravelLog.AmountOfHousesVisitedAtLeastOnce(route("^v^v^v^v^v")) == 2)
  }

  def route(r: String): List[SantaDirection] = {
    r.map(SantaDirection.Parse).toList
  }
}

class Day3SolutionTests extends BaseSolutionTests {
  test("get number of houses visited") {
    dontRunSolutionAutomatically {
      Day3Solution.HousesVisitedAtLeastOnce()
      // res191: Int = 2081
    }
  }

  test("get number of houses visited for santa and robo-santa") {
    dontRunSolutionAutomatically {
      Day3Solution.HousesVisitedBySantaAndRoboSanta()
      // res202: Int = 2341
    }
  }
}
