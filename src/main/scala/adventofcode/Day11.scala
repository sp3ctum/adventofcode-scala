import scala.annotation.tailrec

// --- Day 11: Corporate Policy ---
//
// Santa's previous password expired, and he needs help choosing a new one.
//
// To help him remember his new password after the old one expires, Santa has
// devised a method of coming up with a password based on the previous one.
// Corporate policy dictates that passwords must be exactly eight lowercase
// letters (for security reasons), so he finds his new password by incrementing
// his old password string repeatedly until it is valid.
//
// Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so
// on. Increase the rightmost letter one step; if it was z, it wraps around to
// a, and repeat with the next letter to the left until one doesn't wrap around.
//
// Unfortunately for Santa, a new Security-Elf recently started, and he has
// imposed some additional password requirements:
//
// - Passwords must include one increasing straight of at least three letters,
// like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd
// doesn't count.
// - Passwords may not contain the letters i, o, or l, as these letters can be
// mistaken for other characters and are therefore confusing.
// - Passwords must contain at least two different, non-overlapping pairs of
// letters, like aa, bb, or zz.
//
// For example:
//
// - hijklmmn meets the first requirement (because it contains the straight hij)
// but fails the second requirement requirement (because it contains i and l).
// - abbceffg meets the third requirement (because it repeats bb and ff) but
// fails the first requirement.
// - abbcegjk fails the third requirement, because it only has one double letter
// (bb).
// - The next password after abcdefgh is abcdffaa.
// - The next password after ghijklmn is ghjaabcc, because you eventually skip
// all the passwords that start with ghi..., since i is not allowed.
//
// Given Santa's current password (your puzzle input), what should his next
// password be?

object SantaPasswordCreator {
  val characters: Map[Char,Int] =
    "abcdefghijklmnopqrstuvwxyz".zipWithIndex.toMap

  val numbers: Map[Int,Char] = characters.map{_.swap}
  val base = characters.size

  def incrementPassword(p: String): String = {
    val digits = convertToDigits(p)
    val incremented = digits.sum + 1
    convertToString(incremented, padTo = digits.length)
  }

  def convertToDigits(s: String): List[Int] = {
    s.reverse.zipWithIndex
      .map{case (c, index) =>
        characters(c) * Math.pow(base.toDouble, index.toDouble).toInt
      }
      .toList
  }

  def convertToString(n: Int, padTo: Int): String = {
    val digits = BaseConverter.toBase(n, base = base)
    val firstCharacter = numbers(0).toString()
    val padding: String = firstCharacter * (padTo - digits.length)
    padding + digits.map(i => numbers(i)).mkString
  }
}

object BaseConverter {
  def toBase(n: Int, base: Int): List[Int] = {
    require(base > 1)

    @tailrec
    def divide(result: List[Int], number: Int): List[Int] = {
      val (quotient, remainder) = (number / base, number % base)
      val newResult = remainder :: result

      if (quotient == 0)
        newResult
      else
        divide(newResult, quotient)
    }

    divide(List(), n)
  }
}

object Day11Solution {
  val input = "hepxcrrq"
}
