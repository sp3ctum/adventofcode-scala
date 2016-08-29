import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

class LastElementOfListTests extends FunSuite {

  test("last element of list") {
    assert(LastElementOfList.last(List(1,2)) == 2)
  }

  test("blow up when no elements") {
    intercept[NoSuchElementException] {
      LastElementOfList.last(List())
    }
  }

  test("last element of list recursively") {
    assert(LastElementOfList.lastRecursive(List(1, 2)) == 2)
  }

  test("last element of list recursively, indexed (stupid implementation but good for practice)") {
    assert(LastElementOfList.lastRecursiveIndexed(List(1, 2)) == 2)
  }
}

class OneLinerListSolutionsTests extends FunSuite with Timeouts {
  test("last but one element") {
    assert(OneLinerListSolutions.penultimate(List(1, 2, 3)) == 2)
  }

  test("nth element") {
    val l = List.range(1, 10)
    assert(OneLinerListSolutions.nth(l, 2) == 3)
  }

  test("count list length") {
    assert(OneLinerListSolutions.count(List.range(1, 10)) == 9)
  }

  test("reverse list") {
    assert(OneLinerListSolutions.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("is a list a palindrome") {
    assert(OneLinerListSolutions.isPalindrome(List(1, 2, 3, 2, 1)))
    assert(OneLinerListSolutions.isPalindrome(List(1, 2, 3)) == false)
  }

  test("flatten a list of lists") {
    val nestedList = List(List(1, 2), 3, 4, List(5, 6))
    assert(OneLinerListSolutions.flatten(nestedList) == List(1, 2, 3, 4, 5, 6))
  }

  test("eliminate duplicates") {
    assert(OneLinerListSolutions.eliminateDuplicates(List(1)) == List(1))
    assert(OneLinerListSolutions.eliminateDuplicates(List(1, 2)) == List(1, 2))
    assert(OneLinerListSolutions.eliminateDuplicates(List(1, 2, 2)) == List(1, 2))
    assert(OneLinerListSolutions.eliminateDuplicates(List(1, 1, 2, 3, 3)) == List(1, 2, 3))
  }

  test("duplicates to sublists") {
    assert(OneLinerListSolutions.duplicatesToSublists(List(1, 2, 2)) == List(List(1), List(2, 2)))
    assert(OneLinerListSolutions.duplicatesToSublists(List(1, 1, 2, 3, 3)) == List(List(1, 1), List(2), List(3, 3)))
  }

  test("run-length encoding") {
    assert(OneLinerListSolutions.runLengthEncode(List(1)) == List((1, 1)))
    assert(OneLinerListSolutions.runLengthEncode(List(1, 1)) == List((2, 1)))
    assert(OneLinerListSolutions.runLengthEncode(List(1, 1, 2, 2, 3)) == List((2, 1),
                                                                              (2, 2),
                                                                              (1, 3)))
  }

  test("modified run-length encoding") {
    import OneLinerListSolutions.{RunLengthEncoded, One, Many}

    assert(OneLinerListSolutions.modifiedRunLengthEncoding(List(1)) == List(One(1)))
    assert(OneLinerListSolutions.modifiedRunLengthEncoding(List(1, 1)) == List(Many(1, 2)))
    assert(OneLinerListSolutions.modifiedRunLengthEncoding(List(1, 1, 2, 3, 3)) == List(Many(1, 2), One(2), Many(3, 2)))
  }

  test("run-length decoding") {
    assert(OneLinerListSolutions.decodeRunLengthEncoded(List((3, 1))) == List(1, 1, 1))
    assert(OneLinerListSolutions.decodeRunLengthEncoded(List((3, 1), (2, 2), (1, 3))) == List(1, 1, 1, 2, 2, 3))
  }

  test("run-length encoding (direct version)") {
    failAfter(1 second) { // guard against an endless loop, never failed while
                          // writing though
      assert(OneLinerListSolutions.runLengthEncodeDirectSolution(List(1)) == List((1, 1)))
      assert(OneLinerListSolutions.runLengthEncodeDirectSolution(List(1, 2)) == List((1, 1), (1, 2)))

      assert(OneLinerListSolutions.runLengthEncodeDirectSolution(List(1, 1, 1, 1)) == List((4, 1)))
      assert(OneLinerListSolutions.runLengthEncodeDirectSolution(List(1, 2, 2, 3)) == List((1, 1), (2, 2), (1, 3)))
    }
  }

  test("duplicate a list") {
    assert(OneLinerListSolutions.duplicate(List(1, 2)) == List(1, 2, 1, 2))
  }

  test("duplicate a list a given number of times") {
    assert(OneLinerListSolutions.duplicateTimes(List(1, 2), 3) == List(1, 2, 1, 2, 1, 2))
  }

  test("drop every nth element of a list") {
    assert(OneLinerListSolutions.dropEveryNthElement(List(1, 2), 2) == List(1))
    assert(OneLinerListSolutions.dropEveryNthElement(List.range(1, 11), 2) == List(1, 3, 5, 7, 9))
  }

  test("split a list into two parts") {
    val input = "abcdefghijk"
    assert(OneLinerListSolutions.splitIntoTwoParts(3, input)
             == (("abc".toList, "defghijk".toList)))
  }

  test("slice a list") {
    assert(OneLinerListSolutions.slice(0, 1, List(1, 2)) == List(1))
  }

  test("rotate list to the left") {
    // this one is from the example
    assert(OneLinerListSolutions.rotateToLeft(3, "abcdefghijk".toList)
             == "defghijkabc".toList)
  }

  test("remove from list at given index") {
    assert(OneLinerListSolutions.removeAt(2, List(1, 2, 3, 4)) == List(1, 2, 4))
  }

  test("insert at given index") {
    assert(OneLinerListSolutions.insertAt(2, List(1, 2, 3, 4), 2) == List(1, 2, 2, 3, 4))
  }

  test("create a list from a range") {
    assert(OneLinerListSolutions.range(0, 5) == List(0, 1, 2, 3, 4))
  }

  test("extract random numbers from a list") {
    val list = List(1, 2, 3, 4, 5)
    val randoms = OneLinerListSolutions.randoms(list, 5)
    assert(randoms.length == 5)
  }

  test("Lotto: draw N different random numbers from a list") {
    val result = OneLinerListSolutions.lotto(100, 100)

    // All values should be different.
    // Should also have all values present, since 100 different numbers from 100 is 100.
    assert(result.toSet == (1 to 100).toSet)
  }

  test("random permutation of a list") {
    assert(OneLinerListSolutions.randomPermutation(List(1, 2, 3)).length == 3)
  }

  test("all combinations of distinct objects") {
    assert(OneLinerListSolutions.combinations(List(1, 2, 3)).length == 6)
  }
}
