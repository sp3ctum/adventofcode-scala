import scala.annotation.tailrec

// --- Day 9: All in a Single Night ---
//
// Every year, Santa manages to deliver all of his presents in a single night.
//
// This year, however, he has some new locations to visit; his elves have
// provided him the distances between every pair of locations. He can start and
// end at any two (different) locations he wants, but he must visit each
// location exactly once. What is the shortest distance he can travel to achieve
// this?
//
// For example, given the following distances:
//
// London to Dublin = 464
// London to Belfast = 518
// Dublin to Belfast = 141
//
// The possible routes are therefore:
//
// Dublin -> London -> Belfast = 982
// London -> Dublin -> Belfast = 605
// London -> Belfast -> Dublin = 659
// Dublin -> Belfast -> London = 659
// Belfast -> Dublin -> London = 605
// Belfast -> London -> Dublin = 982
//
// The shortest of these is London -> Dublin -> Belfast = 605, and so the answer
// is 605 in this example.
//
// What is the distance of the shortest route?

case class Route(start: String, end: String, length: Int)
object Route {
  def parse(s: String): Route = {
    val distance = "^(\\w+) to (\\w+) = (\\d+)$".r
    s match {
      case distance(start, end, length) => Route(start, end, length.toInt)
    }
  }
}

object TravellingSalesmanSolver {
  // Avoid premature optimization by trying out the simplest algorithm first.
  // According to wikipedia, a brute force algorithm should be practical for <20
  // cities.
  // In the problem input we have only 7 cities.
  def solveWithBruteForce(routes: List[Route]): List[Route] = {
    ???
  }
}

object Day9Solutions {
  def parseInput(): List[Route] = {
    val input = InputReader.ReadInput("/Day9.txt").split("\n")
    input.map(Route.parse).toList
  }
}
