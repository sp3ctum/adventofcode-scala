import scala.annotation.tailrec

class LastElementOfList

object LastElementOfList {
  def last[A](l: List[A]) = l.last

  def lastRecursive[A](l: List[A]) = {
    @tailrec
    def getLast(l: List[A]): A =
      if (l.size == 1) l.head
      else getLast(l.tail)

    getLast(l)
  }

  @tailrec
  def lastRecursiveIndexed[A](l: List[A], index: Long = 0): A = {
    if (index == l.size) l.head
    else lastRecursiveIndexed(l.tail, index + 1)
  }
}

class LastButOneElementOfList
object LastButOneElementOfList {
  def penultimate[A](l: List[A]): A = {
    val index = l.length - 2
    l(index)
  }
}
