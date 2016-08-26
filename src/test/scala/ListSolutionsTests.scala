import org.scalatest.WordSpec

class ListSolutionsTests extends WordSpec {
  "last element of list" should {
    "find last element" in {
      assert(ListSolutions.last(List(1,2)) == 2)
    }

    "blow up when no elements" in {
      intercept[NoSuchElementException] {
        ListSolutions.last(List())
      }
    }
  }

  "last element of list recursively" should {
    "find last element" in {
      assert(ListSolutions.lastRecursive(List(1, 2)) == 2)
    }
  }

  "last element of list recursively, indexed (stupid implementation but good for practice)" should {
    "find last element" in {
      assert(ListSolutions.lastRecursiveIndexed(List(1, 2)) == 2)
    }
  }
}
