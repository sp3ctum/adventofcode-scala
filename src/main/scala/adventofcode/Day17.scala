// --- Day 17: No Such Thing as Too Much ---
//
// The elves bought too much eggnog again - 150 liters this time. To fit it all
// into your refrigerator, you'll need to move it into smaller containers. You
// take an inventory of the capacities of the available containers.
//
// For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters.
// If you need to store 25 liters, there are four ways to do it:
//
// - 15 and 10
// - 20 and 5 (the first 5)
// - 20 and 5 (the second 5)
// - 15, 5, and 5
//
// Filling all containers entirely, how many different combinations of
// containers can exactly fit all 150 liters of eggnog?

object Day17Solution {
  val containerSizes = List(50, 44, 11, 49, 42, 46, 18, 32, 26, 40, 21, 7, 18, 43, 10, 47, 36, 24, 22, 40)

  def solve(): Int = combinationsOfContainers(containerSizes, 150).length

  def combinationsOfContainers(containers: List[Int], totalSize: Int): Iterator[List[(Int, Int)]] = {
    // have to make the sizes unique so they are all taken into account
    // otherwise containers with the same size appear only once
    val uniqueContainers = containers.zipWithIndex
    for {
      i <- Iterator.range(1, containers.length)
      combination <- uniqueContainers.combinations(i)
      if combination.map{case (n,index) => n}.sum == totalSize
    } yield {
      combination.toList
    }
  }
}
