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
    val cookie = List(44 -> butterscotch,
                      56 -> cinnamon)

    assert(CookieRecipeComparer.sumOf(cookie, i => i.capacity) == 44*(-1) + 56*2)
    assert(CookieRecipeComparer.sumOf(cookie, i => i.durability) == 44*(-2) + 56*3)
    assert(CookieRecipeComparer.sumOf(cookie, i => i.flavor) == 44*6 + 56*(-2))
    assert(CookieRecipeComparer.sumOf(cookie, i => i.texture) == 44*3 + 56*(-1))

    assert(CookieRecipeComparer.score(cookie) == 68 * 80 * 152 * 76)
  }

  test("can find correct order of ingredient amounts") {
    val amounts = CookieRecipeComparer.hundredInGroups(4)
    assert(amounts.contains(List(27,27,15,31)))
  }

  test("get best combination of ingredients") {
    val best = CookieRecipeComparer.bestCombinationOfIngredients(Array(butterscotch, cinnamon))
    val expected = (62842880, List(44 -> butterscotch, 56 -> cinnamon))

    assert(best == expected)
  }

  test("count amount of calories in a cookie") {
    val cookie = List(40 -> butterscotch, 60 -> cinnamon)

    assert(CookieRecipeComparer.has500Calories(cookie) == true)
    assert(CookieRecipeComparer.score(cookie) == 57600000)
  }

  test("get best cookie at 500 calories") {
    val bestCookie = CookieRecipeComparer.bestCookieAt500Calories(Array(butterscotch, cinnamon))
    assert(bestCookie == ((57600000, List(40 -> butterscotch, 60 -> cinnamon))))
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
      // res2: (Int, List[Int,Ingredient]) =
      //     (13882464, List(28 -> Ingredient(Sprinkles,5,-1,0,0,5),
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
    // Elapsed time: 799252777ns
    // res32: (Int, List[(Int, Ingredient)]) =
    //     (11171160, List((27,Ingredient(Sprinkles,5,-1,0,0,5)),
    //                     (27,Ingredient(PeanutButter,-1,3,0,0,1)),
    //                     (15,Ingredient(Frosting,0,-1,4,0,6)),
    //                     (31,Ingredient(Sugar,-1,0,0,2,8))))
  }
}
