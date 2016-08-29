import org.scalatest.FunSuite

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

class OneLinerListSolutionsTests extends FunSuite {
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
}
