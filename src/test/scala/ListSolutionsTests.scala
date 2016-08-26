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

class LastButOneElementOfListTests extends FunSuite {
  test("last but one element") {
    assert(LastButOneElementOfList.penultimate(List(1, 2, 3)) == 2)
  }
}

class NthElementOfListTests extends FunSuite {
  test("nth element") {
    val l = List.range(1, 10)
    assert(NthElementOfList.nth(l, 2) == 3)
  }
}
