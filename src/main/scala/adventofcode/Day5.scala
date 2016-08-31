// --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
// Santa needs help figuring out which strings in his text file are naughty or
// nice.
//
// A nice string is one with all of the following properties:
//
// - It contains at least three vowels (aeiou only), like aei, xazegov, or
//   aeiouaeiouaeiou.
// - It contains at least one letter that appears twice in a row, like xx, abcdde
//   (dd), or aabbccdd (aa, bb, cc, or dd).
// - It does not contain the strings ab, cd, pq, or xy, even if they are part of
//   one of the other requirements.
//
// For example:
//
// - ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...),
//   a double letter (...dd...), and none of the disallowed substrings.
// - aaa is nice because it has at least three vowels and a double letter, even
//   though the letters used by different rules overlap.
// - jchzalrnumimnmhp is naughty because it has no double letter.
// - haegwjzuvuyypxyu is naughty because it contains the string xy.
// - dvszwmarrgswjxmb is naughty because it contains only one vowel.
//
// How many strings are nice?

object NiceStringDeterminer {
  def IsNice(s: String): Boolean = {
    (! ContainsDisallowedString(s)) &&
      ContainsThreeRequiredVowels(s) &&
      ContainsDoubleLetter(s)
  }

  def ContainsThreeRequiredVowels(s: String): Boolean = {
    s.filter(c => "aeiou".exists{c == _})
      .length() >= 3
  }

  def ContainsDoubleLetter(s: String): Boolean = {
    s.sliding(2)
      .exists(pair => pair.head == pair.last)
  }

  def ContainsDisallowedString(s: String): Boolean = {
    List("ab", "cd", "pq", "xy").exists(s.contains(_))
  }
}

object Day5Solution {
  def HowManyStringsAreNice(): Int = {
    val input = GetInput()
    input.filter(NiceStringDeterminer.IsNice)
      .length
  }

  private def GetInput(): Array[String] = {
    InputReader.ReadInput("/Day5.txt").split("\n")
  }
}
