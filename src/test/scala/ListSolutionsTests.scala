import org.scalatest.WordSpec

class LastElementOfListTests extends WordSpec {
  "last element of list" should {
    "find last element" in {
      assert(LastElementOfList.last(List(1,2)) == 2)
    }

    "blow up when no elements" in {
      intercept[NoSuchElementException] {
        LastElementOfList.last(List())
      }
    }
  }

  "last element of list recursively" should {
    "find last element" in {
      assert(LastElementOfList.lastRecursive(List(1, 2)) == 2)
    }
  }

  "last element of list recursively, indexed (stupid implementation but good for practice)" should {
    "find last element" in {
      assert(LastElementOfList.lastRecursiveIndexed(List(1, 2)) == 2)
    }
  }
}
