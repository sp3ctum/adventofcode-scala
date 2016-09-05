import scala.util.matching.Regex

// --- Day 8: Matchsticks ---
//
// Space on the sleigh is limited this year, and so Santa will be bringing his
// list as a digital copy. He needs to know how much space it will take up when
// stored.
//
// It is common in many programming languages to provide a way to escape special
// characters in strings. For example, C, JavaScript, Perl, Python, and even PHP
// handle special characters in very similar ways.
//
// However, it is important to realize the difference between the number of
// characters in the code representation of the string literal and the number of
// characters in the in-memory string itself.
//
// For example:
//
// - "" is 2 characters of code (the two double quotes), but the string contains
// zero characters.
// - "abc" is 5 characters of code, but 3 characters in the string data.
// - "aaa\"aaa" is 10 characters of code, but the string itself contains six "a"
// characters and a single, escaped quote character, for a total of 7 characters
// in the string data.
// - "\x27" is 6 characters of code, but the string itself contains just one -
// an apostrophe ('), escaped using hexadecimal notation.
//
// Santa's list is a file that contains many double-quoted string literals, one
// on each line. The only escape sequences used are \\ (which represents a
// single backslash), \" (which represents a lone double-quote character), and
// \x plus two hexadecimal characters (which represents a single character with
// that ASCII code).
//
// Disregarding the whitespace in the file, what is the number of characters of
// code for string literals minus the number of characters in memory for the
// values of the strings in total for the entire file?
//
// For example, given the four strings above, the total number of characters of
// string code (2 + 5 + 10 + 6 = 23) minus the total number of characters in
// memory for string values (0 + 3 + 7 + 1 = 11) is 23 - 11 = 12.
//

case class EscapedCharacters(val count: Int,
                             val singleMatchLength: Int) {
  val totalLength = count * singleMatchLength
}

object LengthCounter {
  def countOfSerializedCharacters(s: String): Int = {
    s.length()
  }

  def charactersInMemory(s: String): String = {
    var result = s
    // remove first and last quote
    result = result.tail.init

    // try to be clever: every escaped group is worth one character in length
    // (any character will do)
    result = remove(result, escapedDoubleQuote)
    result = remove(result, escapedHexadecimal)
    result = remove(result, escapedBackslash)

    result
  }

  def countOfCharactersInMemory(s: String): Int = {
    val result = charactersInMemory(s)
    result.length()
  }

  def charsInCodeMinusCharsInMemory(s: List[String]): Int = {
    val charCount = s.map(countOfSerializedCharacters).sum
    val memoryCount = s.map(countOfCharactersInMemory).sum

    charCount - memoryCount
  }

  private def remove(s: String, r: Regex): String = {
    r.replaceAllIn(s, ".")
  }

  val escapedDoubleQuote: Regex = """\\"""".r
  val escapedBackslash: Regex = """\\\\""".r

  val hexChar: Regex = """[0-9a-f]""".r
  val escapedHexadecimal: Regex = s"""\\\\x${hexChar}${hexChar}""".r
}

object Day8Solution {
  def Solve(): Int = {
    val input = readInput()
    LengthCounter.charsInCodeMinusCharsInMemory(input)
  }

  def readInput(): List[String] = {
    InputReader.ReadInput("/Day8.txt").split("\n").toList
  }
}
