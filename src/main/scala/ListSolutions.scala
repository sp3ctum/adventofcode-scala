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

object OneLinerListSolutions {
  // P02 (*) Find the last but one element of a list.
  def penultimate[A](l: List[A]): A = l.init.last

  // P03 (*) Find the Kth element of a list.
  def nth[A](l: List[A], i: Int): A = l(i)

  // P04 (*) Find the number of elements of a list.
  def count[A](l: List[A]) = l.length

  // P05 (*) Reverse a list.
  def reverse[A](l: List[A]) = l.reverse

  // P06 (*) Find out whether a list is a palindrome.
  def isPalindrome[A](l: List[A]) = l == l.reverse

  // P07 (**) Flatten a nested list structure.
  def flatten[A](result: List[Any]): List[Any] = {
    result match {
      case Nil => result
      case (l: List[Any]) :: rest => flatten(l) ::: flatten(rest)
      case l :: rest => l :: flatten(rest)
    }
  }

  // P08 (**) Eliminate consecutive duplicates of list elements.
  def eliminateDuplicates[A](l: List[A]): List[A] = {
    @tailrec
    def eliminate(result: List[A], previousItem: A, remaining: List[A]): List[A] = {
      remaining match {
        case Nil => result
        case x :: rest =>
          if (previousItem == x)
            // skip this item
            eliminate(result, x, rest)
          else
            eliminate(x :: result, x, rest)
      }
    }

    eliminate(result = List(l.head), previousItem = l.head, remaining = l.tail)
      .reverse
  }

  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  def duplicatesToSublists[T](remaining: List[T]): List[List[T]] = {
    remaining match {
      case Nil => Nil
      case x :: xs => {
        val (sames, rest) = xs span{x == _}
        (x :: sames) :: duplicatesToSublists(rest)
      }
    }
  }

  // P10 (*) Run-length encoding of a list.
  def runLengthEncode[T](l: List[T]): List[(Int, T)] = {
    duplicatesToSublists(l)
      .map(groupOfItems => (groupOfItems.length, groupOfItems.head))
  }

  // P11 (*) Modified run-length encoding.
  //
  // Modify the result of problem P10 in such a way that if an element has no
  // duplicates it is simply copied into the result list. Only elements with
  // duplicates are transferred as (N, E) terms. Example:

  // scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  abstract class RunLengthEncoded[T]
  case class One[T](value: T) extends RunLengthEncoded[T]
  case class Many[T](value: T, count: Int) extends RunLengthEncoded[T]

  def modifiedRunLengthEncoding[T](l: List[T]): List[RunLengthEncoded[T]] = {
    runLengthEncode(l)
      .map (group => group match {
              case (1, value) => One(value)
              case (count, value) => Many(value, count)
            })
  }

  // P12 (**) Decode a run-length encoded list.
  // Given a run-length code list generated as specified in problem P10, construct
  //   its uncompressed version. Example:
  // scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  // res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  def decodeRunLengthEncoded[T](l: List[(Int, T)]): List[T] = {
    l.map(group => {
            val (count, item) = group
            List.fill(count)(item)
          })
      .flatten
  }
}
