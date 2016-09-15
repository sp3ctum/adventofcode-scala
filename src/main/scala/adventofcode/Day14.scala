import scala.collection.immutable.Stream

// --- Day 14: Reindeer Olympics ---
//
// This year is the Reindeer Olympics! Reindeer can fly at high speeds, but must
// rest occasionally to recover their energy. Santa would like to know which of
// his reindeer is fastest, and so he has them race.
//
// Reindeer can only either be flying (always at their top speed) or resting
// (not moving at all), and always spend whole seconds in either state.
//
// For example, suppose you have the following Reindeer:
//
// Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
// Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
//
// After one second, Comet has gone 14 km, while Dancer has gone 16 km. After
// ten seconds, Comet has gone 140 km, while Dancer has gone 160 km. On the
// eleventh second, Comet begins resting (staying at 140 km), and Dancer
// continues on for a total distance of 176 km. On the 12th second, both
// reindeer are resting. They continue to rest until the 138th second, when
// Comet flies for another ten seconds. On the 174th second, Dancer flies for
// another 11 seconds.
//
// In this example, after the 1000th second, both reindeer are resting, and
// Comet is in the lead at 1120 km (poor Dancer has only gotten 1056 km by that
// point). So, in this situation, Comet would win (if the race ended at 1000
// seconds).
//
// Given the descriptions of each reindeer (in your puzzle input), after exactly
// 2503 seconds, what distance has the winning reindeer traveled?
//
case class Reindeer(name: String, flying: Flying, restTime: Int)
case class Flying(speed: Int, time: Int)

object ReindeerDistanceMeasurer {
  def distanceAtSeconds(n: Int, r: Reindeer): Int = {
    val raceAtOneSecondIncrements = reindeerRace(r)
    raceAtOneSecondIncrements(n)
  }

  def reindeerRace(r: Reindeer): Stream[Int] = {
    // I think I'm supposed to be building an iterator in this exercise, but
    // their internal state can get confusing. I prefer a state machine with
    // mutually recursive functions; it's much more expressive
    def fly(distance: Int): Stream[Int] = {
      val flying = Stream.iterate(distance + r.flying.speed, r.flying.time)(d => d + r.flying.speed)
      flying append rest(flying.last)
    }

    def rest(distance: Int): Stream[Int] = {
      val resting = Stream.iterate(distance, r.restTime)(d => d)
      resting append fly(resting.last)
    }

    // starts at 0 seconds, so distance will be zero
    0 #:: fly(0)
  }
}

object Day14Solution {
  def readInput(): List[String] = {
    InputReader.ReadInput("Day14.txt").split("\n").toList
  }

  def parseInput(): List[Reindeer] = {
    val input = readInput()
    val reindeer = """^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.$""".r

    input.map{
      case reindeer(name, flyspeed, flytime, restTime) =>
        Reindeer(name, Flying(flyspeed.toInt, flytime.toInt), restTime.toInt)
    }
  }

  def solve(): (Reindeer, Int) = {
    val reindeers = parseInput()
    val finishTime = 2503

    reindeers
      .map(r => (r, ReindeerDistanceMeasurer.distanceAtSeconds(finishTime, r)))
      .maxBy{case (r,distance) => distance}
  }

  // --- Part Two ---
  //
  // Seeing how reindeer move in bursts, Santa decides he's not pleased with the
  // old scoring system.
  //
  // Instead, at the end of each second, he awards one point to the reindeer
  // currently in the lead. (If there are multiple reindeer tied for the lead,
  // they each get one point.) He keeps the traditional 2503 second time limit,
  // of course, as doing otherwise would be entirely ridiculous.
  //
  // Given the example reindeer from above, after the first second, Dancer is in
  // the lead and gets one point. He stays in the lead until several seconds
  // into Comet's second burst: after the 140th second, Comet pulls into the
  // lead and gets his first point. Of course, since Dancer had been in the lead
  // for the 139 seconds before that, he has accumulated 139 points by the 140th
  // second.
  //
  // After the 1000th second, Dancer has accumulated 689 points, while poor
  // Comet, our old champion, only has 312. So, with the new scoring system,
  // Dancer would win (if the race ended at 1000 seconds).
  //
  // Again given the descriptions of each reindeer (in your puzzle input), after
  // exactly 2503 seconds, how many points does the winning reindeer have?
  def solvePart2() = {
    val reindeers = parseInput()
    val finishTime = 2503
    val points = ReindeerPointCalculator.getRaceStatusAtSeconds(reindeers, finishTime)
    points.toList.maxBy {case (r, points) => points}
  }
}

object ReindeerPointCalculator {
  def getRaceLeaderAtSeconds(reindeers: List[Reindeer], time: Int): (Reindeer,Int) = {
    val points = getRaceStatusAtSeconds(reindeers, time)
    points.toList.sortWith {
      case ((_reindeer,points), (_, points2)) => points > points2
    }.head
  }

  def getRaceStatusAtSeconds(reindeers: List[Reindeer], finishTime: Int): Map[Reindeer, Int] = {
    val races = reindeerRaces(reindeers)
    val startPoints = reindeers.map(r => (r, 0)).toMap

    val time = Iterator.from(0).take(finishTime)

    time.foldLeft(startPoints)((points, time) => addPointsForLeaders(points, time, races))
  }

  private def reindeerRaces(reindeers: List[Reindeer]): Map[Reindeer, Stream[Int]] = {
    reindeers.zip(reindeers.map(ReindeerDistanceMeasurer.reindeerRace)).toMap
  }

  private def addPointsForLeaders(points: Map[Reindeer, Int],
                                  time: Int,
                                  races: Map[Reindeer, Stream[Int]]): Map[Reindeer, Int] = {
    val positionsWithReindeers = races.groupBy{case (r, race) => race(time + 1)}.mapValues(_.keys)
    val (_, reindeersInTheLead) = positionsWithReindeers.maxBy { case (distance, reindeers) => distance }

    reindeersInTheLead.foldLeft(points)(
      (result, reindeer) => {
        val newPoints: Int = result(reindeer) + 1
        result + (reindeer -> newPoints)
      }
    )
  }
}
