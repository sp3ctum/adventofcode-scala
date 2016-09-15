import java.lang.Math
import scala.annotation.tailrec

// --- Day 15: Science for Hungry People ---
//
// Today, you set out on the task of perfecting your milk-dunking cookie recipe.
// All you have to do is find the right balance of ingredients.
//
// Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a
// list of the remaining ingredients you could use to finish the recipe (your
// puzzle input) and their properties per teaspoon:
//
// - capacity (how well it helps the cookie absorb milk)
// - durability (how well it keeps the cookie intact when full of milk)
// - flavor (how tasty it makes the cookie)
// - texture (how it improves the feel of the cookie)
// - calories (how many calories it adds to the cookie)
//
// You can only measure ingredients in whole-teaspoon amountOptions accurately, and
// you have to be accurate so you can reproduce your results in the future. The
// total score of a cookie can be found by adding up each of the properties
// (negative totals become 0) and then multiplying together everything except
// calories.
//
// For instance, suppose you have these two ingredients:
//
// Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
//
// Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
//
// Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of
// cinnamon (because the amountOptions of each ingredient must add up to 100) would
// result in a cookie with the following properties:
//
// - A capacity of 44*-1 + 56*2 = 68
// - A durability of 44*-2 + 56*3 = 80
// - A flavor of 44*6 + 56*-2 = 152
// - A texture of 44*3 + 56*-1 = 76
//
// Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now)
// results in a total score of 62842880, which happens to be the best score
// possible given these ingredients. If any properties had produced a negative
// total, it would have instead become zero, causing the whole score to multiply
// to zero.
//
// Given the ingredients in your kitchen and their properties, what is the total
// score of the highest-scoring cookie you can make?

case class Ingredient(name: String,
                      capacity: Int,
                      durability: Int,
                      flavor: Int,
                      texture: Int,
                      calories: Int)

object Ingredient {
  type IngredientName = String
  def parse(s: String): Ingredient = {
    val n = "([0-9-]+)".r
    val ingredients = s"(\\w+): capacity $n, durability $n, flavor $n, texture $n, calories $n".r
    s match {
      case ingredients(name, capacity, durability, flavor, texture, calories) =>
        Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
    }
  }
}

object CookieRecipeComparer {
  type CookieIngredients = List[(Int,Ingredient)]

  def score(cookie: CookieIngredients): Int = {
    val total = List(sumOf(cookie, i => i.capacity),
                     sumOf(cookie, i => i.durability),
                     sumOf(cookie, i => i.flavor),
                     sumOf(cookie, i => i.texture)).product
    Math.max(total, 0)
  }

  def has500Calories(cookie: CookieIngredients): Boolean = {
    sumOf(cookie, i => i.calories) == 500
  }

  def sumOf(cookie: CookieIngredients, property: Ingredient => Int): Int = {
    val sum = cookie.map{case (amount,ingredient) =>
      property(ingredient) * amount}.sum
    Math.max(sum, 0)
  }

  def bestCombinationOfIngredients(ingredients: Array[Ingredient]): (Int, CookieIngredients) = {
    val options = cookieOptions(ingredients)

    val cookieScores = options.map(c => CookieRecipeComparer.score(c) -> c)
    cookieScores maxBy {case (score, ingredients) => score}
  }

  def hundredInGroups(groups: Int): IndexedSeq[List[Int]] = {
    groups match {
      case 4 =>
        // This is fast enough to compute by brute force.
        // maximum for a group is 97 - so each of the other three groups would
        // contain the number 1.
        for {a <- (1 to 97)
             b <- (1 to 97)
             c <- (1 to 97)
             d <- (1 to 97)
             if (a + b + c + d) == 100
        }
        yield List(a,b,c,d)
      case 2 => // for testing
        for {a <- (1 to 99)
             b <- (1 to 99)
             if (a + b) == 100}
        yield List(a,b)
    }
  }

  /**
    * all the different kinds of cookies it's possible to make with the given ingredients
    */
  def cookieOptions(ingredients: Array[Ingredient]): IndexedSeq[CookieIngredients] = {
    val amountOptions = hundredInGroups(ingredients.length)
    amountOptions.map(a => a.zip(ingredients))
  }

  def bestCookieAt500Calories(ingredients: Array[Ingredient]): (Int, CookieIngredients) = {
    val cookiesWith500Calories = cookieOptions(ingredients)
      .withFilter(CookieRecipeComparer.has500Calories)

    val cookieScores = cookiesWith500Calories.map(c => CookieRecipeComparer.score(c) -> c)

    cookieScores maxBy {case (score, ingredients) => score}
  }
}

object Day15Solution {
  val input = """
Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1
Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6
Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8""".split("\n").filterNot(_ == "")
  def parseInput = input.map(Ingredient.parse)

  def solve(): (Int, List[(Int, Ingredient)]) = {
    val ingredients = parseInput
    CookieRecipeComparer.bestCombinationOfIngredients(ingredients)
  }

  // --- Part Two ---
  //
  // Your cookie recipe becomes wildly popular! Someone asks if you can make
  // another recipe that has exactly 500 calories per cookie (so they can use it
  // as a meal replacement). Keep the rest of your award-winning process the
  // same (100 teaspoons, same ingredients, same scoring system).
  //
  // For example, given the ingredients above, if you had instead selected 40
  // teaspoons of butterscotch and 60 teaspoons of cinnamon (which still adds to
  // 100), the total calorie count would be 40*8 + 60*3 = 500. The total score
  // would go down, though: only 57600000, the best you can do in such trying
  // circumstances.
  //
  // Given the ingredients in your kitchen and their properties, what is the
  // total score of the highest-scoring cookie you can make with a calorie total
  // of 500?
  def solvePart2(): (Int, List[(Int, Ingredient)]) = {
    val ingredients = parseInput
    CookieRecipeComparer.bestCookieAt500Calories(ingredients)
  }
}
