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

object SantaPasswordIncrementer {
  def nextValidPassword(p: String): String = {
    val passwords = nextPasswordsFor(p)
    val validPasswords = passwords.filter(SantaPasswordValidation.validate)
    validPasswords.take(1).toList.head
  }

  def nextPasswordsFor(p: String): Iterator[String] = {
    Iterator.iterate(p)(SantaPasswordCreator.incrementPassword).drop(1)
  }
}

object SantaPasswordCreator {
  val characters: Map[Char,Int] =
    "abcdefghijklmnopqrstuvwxyz".zipWithIndex.toMap

  val numbers: Map[Int,Char] = characters.map{_.swap}
  val base = numbers.size

  def incrementPassword(p: String): String = {
    incrementCharacter("", true, p.reverse.toList).reverse
  }

  private def incrementCharacter(result: String, shouldIncrement: Boolean, remaining: List[Char]): String = {
    remaining match {
      case Nil => result
      case 'z' :: Nil => "aa" + result
      case 'z' :: rest => incrementCharacter("a" + result, true, rest)
      case x :: rest if (shouldIncrement) => result + nextCharacter(x).toString() + rest.mkString
      case x :: rest => x + result + rest.mkString
    }
  }

  def nextCharacter(c: Char): Char = {
    c match {
      case 'z' => 'a'
      case x => (x + 1).toChar
    }
  }
}

object SantaPasswordValidation {
  def validate(p: String): Boolean = {
    doesNotContainDisallowedLetters(p) &&
      containsThreeSuccessiveCharactersByCharValue(p) &&
      containsTwoDifferentPairs(p)
  }

  def containsThreeSuccessiveCharactersByCharValue(p: String): Boolean = {
    p.toList.sliding(3).exists{
      case chars => {
        val List(charA, charB, charC) = chars
        val next = SantaPasswordCreator.nextCharacter _

        charC == next(charB) && charB == next(charA)
      }
    }
  }

  def doesNotContainDisallowedLetters(p: String): Boolean = {
    ! p.contains("i") && ! p.contains("o") && ! p.contains("l")
  }

  def containsTwoDifferentPairs(p: String): Boolean = {
    val pairs = p.sliding(2).filter(s => s.head == s.last).toList
    val groups = pairs.groupBy(identity).keys
    groups.size >= 2
  }
}

object Day11Solution {
  val input = "hepxcrrq"
  def getSantasNextPassword(): String = {
    SantaPasswordIncrementer.nextValidPassword(input)
  }

  // --- Part Two ---
  //
  // Santa's password expired again. What's the next one?

  def getSantasSecondPassword(): String = {
    val firstPassword = getSantasNextPassword
    SantaPasswordIncrementer.nextValidPassword(firstPassword)
  }
}
