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
}
