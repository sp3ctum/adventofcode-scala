import org.scalatest.FunSuite
import scala.collection.immutable.Stream

class Day15Tests extends FunSuite {
  test("parsing") {
    val input = "Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5"
    val parsed = Ingredient.parse(input)
    // parsed: (Ingredient.CookieName, Ingredient) = ()

    assert(parsed == ((Ingredient("Sprinkles",5,-1,0,0,5))))
  }

  test("calculate cookie score") {
    val score = CookieRecipeComparer.score(Map(44 -> butterscotch,
                                               56 -> cinnamon))
    assert(score == 62842880)
  }

  test("get groups of ingredients") {
    val groups = Day15Solution.ingredientAmounts(2, 10)


    // Timer.time {
    //   Day15Solution.ingredientAmounts(4, 100).length
    // }

    val expected = List(List(9, 1), List(8, 2), List(7, 3), List(6, 4),
                        List(5, 5), List(4, 6), List(3, 7), List(2, 8), List(1, 9))
    assert(groups.toSet == expected.toSet)
  }

  test("get best combination of ingredients") {
    val best = Day15Solution.bestCombinationOfIngredients(Array(butterscotch, cinnamon))
    val expected = (62842880, Map(44 -> butterscotch, 56 -> cinnamon))

    assert(best == expected)
  }

  def butterscotch = Ingredient.parse(
    "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8")
  def cinnamon = Ingredient.parse(
    "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")
}

class Day15SolutionTests extends BaseSolutionTests {
  test("solve part 1: ") {
    dontRunSolutionAutomatically {
      // what is the total score of the highest-scoring cookie you can make?
      Day15Solution.solve()
    }
  }
}
