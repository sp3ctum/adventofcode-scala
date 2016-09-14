
// --- Day 16: Aunt Sue ---
//
// Your Aunt Sue has given you a wonderful gift, and you'd like to send her a
// thank you card. However, there's a small problem: she signed it "From, Aunt
// Sue".
//
// You have 500 Aunts named "Sue".
//
// So, to avoid sending the card to the wrong person, you need to figure out
// which Aunt Sue (which you conveniently number 1 to 500, for sanity) gave you
// the gift. You open the present and, as luck would have it, good ol' Aunt Sue
// got you a My First Crime Scene Analysis Machine! Just what you wanted. Or
// needed, as the case may be.
//
// The My First Crime Scene Analysis Machine (MFCSAM for short) can detect a few
// specific compounds in a given sample, as well as how many distinct kinds of
// those compounds there are. According to the instructions, these are what the
// MFCSAM can detect:
//
// - children, by human DNA age analysis.
// - cats. It doesn't differentiate individual breeds.
// - Several seemingly random breeds of dog: samoyeds, pomeranians, akitas, and vizslas.
// - goldfish. No other kinds of fish.
// - trees, all in one group.
// - cars, presumably by exhaust or gasoline or something.
// - perfumes, which is handy, since many of your Aunts Sue wear a few kinds.
//
// In fact, many of your Aunts Sue have many of these. You put the wrapping from
// the gift into the MFCSAM. It beeps inquisitively at you a few times and then
// prints out a message on ticker tape:
//
// - children: 3
// - cats: 7
// - samoyeds: 2
// - pomeranians: 3
// - akitas: 0
// - vizslas: 0
// - goldfish: 5
// - trees: 3
// - cars: 2
// - perfumes: 1
//
// You make a list of the things you can remember about each Aunt Sue. Things
// missing from your list aren't zero - you simply don't remember the value.
//
// What is the number of the Sue that got you the gift?

object Day16Solution {
  type Fact = (String, Int)

  def readInput(): Array[String] = {
    InputReader.ReadInput("Day16.txt").split("\n")
  }

  def parseInput(lines: Array[String]): Map[Int,Map[String,Int]] = {
    lines.map(parse).toMap
  }

  def parse(s: String): (Int, Map[String, Int]) = {
    val sue = s"^Sue (\\d+): (.+?), (.+?), (.+?)$$".r

    s match {
      case sue(sueNumber, fa, fb, fc) => {
        val facts = parseFacts(fa, fb, fc)
        sueNumber.toInt -> facts.toMap
      }
    }
  }

  def parseFacts(facts: String*): List[Fact] = {
    val fact = "(\\w+?): (\\d+?)".r

    facts
      .map{case fact(k, v) => k -> v.toInt}
      .filter{case (k, v) => v != 0}
      .toList
  }

  def findSue(facts: List[Fact]) = {
    val sues = parseInput(readInput())
    // It's possible no Sue can be found with the given facts.
    // But if we do a fuzzy search we might be able to narrow
    // the result down to just one Sue.
    facts.foldLeft(sues)(
      (result, f) => {
        val (factName, factValue) = f
        result.filter{ case (n, sueFacts) => {
                        val sueFactValue = sueFacts.getOrElse(factName, 0)
                        sueFactValue == 0 || sueFactValue == factValue
                      }}
      }
    )
  }
}
