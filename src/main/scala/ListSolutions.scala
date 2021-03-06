import scala.annotation.tailrec
import scala.util.Random

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
      case (l: List[Any]) :: rest => flatten(l) ::: flatten(rest) // linter:ignore
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
  @tailrec
  def duplicatesToSublists[T](
    remaining: List[T],
    result: List[List[T]] = List()
  ): List[List[T]] = {
    remaining match {
      case Nil => result
      case x :: xs => {
        val (sames, rest) = xs span { x == _ }
        duplicatesToSublists(rest, result ::: List(x :: sames))
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
      .map {
        case (1, value) => One(value)
        case (count, value) => Many(value, count)
      }
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

  // P13 (**) Run-length encoding of a list (direct solution).
  //   Implement the so-called run-length encoding data compression method
  //   directly. I.e. don't use other methods you've written (like P09's pack);
  //   do all the work directly.

  //   Example:
  // scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  //
  def runLengthEncodeDirectSolution[T](l: List[T]): List[(Int, T)] = {
    // Seems like this requires a solution that's not optimal;
    // Meaning it must do everything in the same loop instead of using higher
    // order functions like my version of runLengthEncode.
    def encode(
      result: List[(Int, T)],
      remaining: List[T],
      previousItem: T,
      successiveItemsCount: Int
    ): List[(Int, T)] = {
      remaining match {
        case Nil => (successiveItemsCount, previousItem) :: result
        case newItem :: rest =>
          if (newItem == previousItem)
            encode(result, rest, previousItem, successiveItemsCount + 1)
          else {
            val newResult = (successiveItemsCount, previousItem) :: result
            encode(newResult, rest, newItem, 1)
          }
      }
    }
    encode(List(), l.tail, l.head, successiveItemsCount = 1)
      .reverse
  }

  // P14 (*) Duplicate the elements of a list.
  def duplicate[T](l: List[T]): List[T] = l ::: l

  // P15 (**) Duplicate the elements of a list a given number of times.
  def duplicateTimes[T](l: List[T], times: Int): List[T] =
    List.fill(times)(l).flatten

  // P16 (**) Drop every Nth element from a list.
  def dropEveryNthElement[T](l: List[T], n: Int): List[T] = {
    l.zipWithIndex.collect {
      case (x, i) if ((i + 1) % n != 0) => x
    }
  }

  // P17 (*) Split a list into two parts.
  def splitIntoTwoParts[T](n: Int, s: IndexedSeq[T]) = s.splitAt(n)

  // P18 (**) Extract a slice from a list.
  def slice[T](start: Int, end: Int, l: List[T]): List[T] = l.slice(start, end)

  // P19 (**) Rotate a list N places to the left.
  def rotateToLeft[T](n: Int, l: List[T]): List[T] = {
    val input = l.toVector // optimization

    l.zipWithIndex.map {
      case (x, index) => {
        // overflow will go around and search from the beginning / end
        val newIndex = (index + n) % input.length
        input(newIndex)
      }
    }
  }

  // P20 (*) Remove the Kth element from a list.
  def removeAt[T](zeroBasedIndex: Int, l: List[T]): List[T] = {
    val (before, after) = l.splitAt(zeroBasedIndex + 1)
    before.init ++ after
  }

  // P21 (*) Insert an element at a given position into a list.
  def insertAt[T](zeroBasedIndex: Int, l: List[T], element: T): List[T] = {
    val (before, after) = l.splitAt(zeroBasedIndex)
    before ++ List(element) ++ after
  }

  // P22 (*) Create a list containing all integers within a given range.
  def range(start: Int, end: Int): List[Int] = List.range(start, end)

  // P23 (**) Extract a given number of randomly selected elements from a list.
  def randoms[T](source: List[T]): List[T] = {
    val random = new Random()

    source.map { _ =>
      val index = random.nextInt(source.length)
      source(index)
    }
  }

  // P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  def lotto(amount: Int, poolSize: Int): List[Int] = {
    val uniqueNumbers = (1 to poolSize).toSet.toList
    return Random.shuffle(uniqueNumbers).take(amount)
  }

  // P25 (*) Generate a random permutation of the elements of a list.
  def randomPermutation[T](l: List[T]): List[T] = {
    // this may not be random but I don't care
    l.permutations.next()
  }

  // P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
  def combinations[T](l: List[T]): List[List[T]] = l.permutations.toList
}
