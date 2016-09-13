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
  def score(hundredInFourGroups: Map[Int,Ingredient]): Int = {
    def sumOf(property: Ingredient => Int): Int = {
      val sum = hundredInFourGroups.map{case (amount,ingredient) =>
        property(ingredient) * amount}.sum
      Math.max(0, sum)
    }

    val total = List(sumOf(i => i.capacity),
                     sumOf(i => i.durability),
                     sumOf(i => i.flavor),
                     sumOf(i => i.texture)).product

    Math.max(total, 0)
  }
}

object Day15Solution {
  val input = """
Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1
Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6
Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8""".split("\n").filterNot(_ == "")
  def parseInput = input.map(Ingredient.parse)

  def solve() = {
    val ingredients = parseInput
    bestCombinationOfIngredients(ingredients)
  }

  def bestCombinationOfIngredients(ingredients: Array[Ingredient]): (Int, Map[Int, Ingredient]) = {
    val amountOptions = hundredInFourGroups()
    val cookies = amountOptions.map(a => a.zip(ingredients).toMap)

    val cookieScores = cookies.map(c => CookieRecipeComparer.score(c) -> c)
    cookieScores.sortBy {case (score, ingredients) => score}
      .last
  }

  def hundredInFourGroups(): IndexedSeq[List[Int]] = {
    // this is fast enough to compute by brute force
    for {a <- (1 to 97)
         b <- (1 to 97)
         c <- (1 to 97)
         d <- (1 to 97)
         if (a + b + c + d) == 100}
    yield List(a,b,c,d)
  }
}
