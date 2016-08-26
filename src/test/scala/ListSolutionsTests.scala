import org.scalatest.WordSpec

class ListSolutionsTests extends WordSpec {
  "Processing a list" should {
    "find last element" in {
      assert(ListSolutions.last(List(1,2)) == 2)
    }

    "blow up when no elements" in {
      intercept[NoSuchElementException] {
        ListSolutions.last(List())
      }
    }
  }
}
