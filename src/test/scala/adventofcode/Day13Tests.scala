import org.scalatest.FunSuite

class SittingOrderDeterminerTests extends FunSuite {
  test("happiness for arrangement") {
    // should also calculate happinesses between Alice and Cecilia
    val arrangement = List("Alice", "Bob", "Cecilia")
    val happinesses = Map(("Alice", "Bob") -> 1,
                          ("Bob", "Alice") -> 2,
                          ("Bob", "Cecilia") -> 4,
                          ("Cecilia", "Bob") -> 8,
                          ("Alice", "Cecilia") -> 16,
                          ("Cecilia", "Alice") -> 32)

    assert(SittingOrderDeterminer.happinessForArrangement(arrangement, happinesses)
             == happinesses.values.sum)
  }

  test("calculate collective happiness") {
    val data = """
Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.
""".split("\n").filter{_ != ""}.map(HappinessFactory.parse).toList

    assert(SittingOrderDeterminer.optimalSittingPosition(data) ==
             ((Set("Alice", "Bob", "Carol", "David"),330)))
  }
}

class Day13SolutionTests extends BaseSolutionTests {
  test("parse input") {
    dontRunSolutionAutomatically {
      Day13Solution.parseInput(Day13Solution.readInput())
    }
  }

  test("solve part 1") {
    dontRunSolutionAutomatically {
      Day13Solution.solve()
      // res39: (Set[String], Int) = (Set(Carol, George, Bob, Frank, Eric, Alice, David, Mallory),618)
    }
  }
}
