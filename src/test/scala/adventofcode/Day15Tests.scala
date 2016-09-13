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

  test("get best combination of ingredients") {
    val best = CookieRecipeComparer.bestCombinationOfIngredients(Array(butterscotch, cinnamon))
    val expected = (62842880, Map(44 -> butterscotch, 56 -> cinnamon))

    assert(best == expected)
  }

  test("count amount of calories in a cookie") {
    val cookie = Map(40 -> butterscotch, 60 -> cinnamon)

    assert(CookieRecipeComparer.has500Calories(cookie) == true)
    assert(CookieRecipeComparer.score(cookie) == 57600000)
  }

  test("get best cookie at 500 calories") {
    val bestCookie = CookieRecipeComparer.bestCookieAt500Calories(Array(butterscotch, cinnamon))
    assert(bestCookie == ((57600000, Map(40 -> butterscotch, 60 -> cinnamon))))
  }

  def butterscotch = Ingredient.parse("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8")
  def cinnamon = Ingredient.parse("Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")
}

class Day15SolutionTests extends BaseSolutionTests {
  test("solve part 1 ") {
    dontRunSolutionAutomatically {
      // what is the total score of the highest-scoring cookie you can make?
      Timer.time {
        Day15Solution.solve()
      }
      // Elapsed time: 856902664ns
      // res2: (Int, Map[Int,Ingredient]) =
      //     (13882464, Map(28 -> Ingredient(Sprinkles,5,-1,0,0,5),
      //                    35 -> Ingredient(PeanutButter,-1,3,0,0,1),
      //                    18 -> Ingredient(Frosting,0,-1,4,0,6),
      //                    19 -> Ingredient(Sugar,-1,0,0,2,8)))
    }
  }

  test("solve part 2") {
    // what is the total score of the highest-scoring cookie you can make?
    // this one requires a cookie with exactly 500 calories
    Timer.time {
      Day15Solution.solvePart2()
    }
    // incorrect!!!

    // Elapsed time: 775394286ns
    // res11: (Int, Map[Int,Ingredient]) = (11162880,Map(25 -> Ingredient(Sprinkles,5,-1,0,0,5), 27 -> Ingredient(PeanutButter,-1,3,0,0,1), 18 -> Ingredient(Frosting,0,-1,4,0,6), 30 -> Ingredient(Sugar,-1,0,0,2,8)))
    // incorrect too
    // res12: Int = 11163380


  }
}
