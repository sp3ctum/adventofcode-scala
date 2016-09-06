import scala.annotation.tailrec
import scala.collection.immutable.{ Queue }

// --- Day 10: Elves Look, Elves Say ---
//
// Today, the Elves are playing a game called look-and-say. They take turns
// making sequences by reading aloud the previous sequence and using that
// reading as the next sequence. For example, 211 is read as "one two, two
// ones", which becomes 1221 (1 2, 2 1s).
//
// Look-and-say sequences are generated iteratively, using the previous value as
// input for the next step. For each step, take the previous value, and replace
// each run of digits (like 111) with the number of digits (3) followed by the
// digit itself (1).
//
// For example:
//
// - 1 becomes 11 (1 copy of digit 1).
// - 11 becomes 21 (2 copies of digit 1).
// - 21 becomes 1211 (one 2 followed by one 1).
// - 1211 becomes 111221 (one 1, one 2, and two 1s).
// - 111221 becomes 312211 (three 1s, two 2s, and one 1).
//
// Starting with the digits in your puzzle input, apply this process 40 times.
// What is the length of the result?

object LookAndSay {
  type CharCounts = Queue[(Int,Char)]

  def encode(s: String): String = {
    @tailrec
    def getGroups(result: CharCounts, remaining: Seq[Char]): CharCounts = {
      remaining match {
        case Seq() => result
        case Seq(x, rest @ _*) => {
          val (sames, others) = rest.span{_ == x}
          getGroups(result.enqueue((sames.length + 1, x)), others)
        }
      }
    }

    val groups = getGroups(Queue(), s)
    groups.map{case (count, c) => s"${count}${c}"}.mkString
  }

  def encodeTimes(s: String, n: Int): String = {
    (1 to n).foldLeft(s)((result, _i) => encode(result))
  }
}

object Day10Solution {
  val input = "1113222113"
  def solve(): String = {
    LookAndSay.encodeTimes(input, 40)
  }

  def solvePart2(): String = {
    LookAndSay.encodeTimes(input, 50)
  }
}
