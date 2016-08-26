import scala.annotation.tailrec

// P01 (*) Find the last element of a list.
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

// P02 (*) Find the last but one element of a list.
object LastButOneElementOfList {
  def penultimate[A](l: List[A]): A = l.init.last
}

// P03 (*) Find the Kth element of a list.
object NthElementOfList {
  def nth[A](l: List[A], i: Int): A = l(i)
}

// P04 (*) Find the number of elements of a list.
object NumberOfElements {
  def count[A](l: List[A]) = l.length
}
