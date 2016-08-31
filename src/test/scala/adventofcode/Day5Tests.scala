import org.scalatest.FunSuite

class Day5Tests extends FunSuite {
  test("disallowed strings") {
    assert(! NiceStringDeterminer.ContainsDisallowedString("this is a nice string"))

    assert(NiceStringDeterminer.ContainsDisallowedString("ab"))
    assert(NiceStringDeterminer.ContainsDisallowedString("cd"))
    assert(NiceStringDeterminer.ContainsDisallowedString("pq"))
    assert(NiceStringDeterminer.ContainsDisallowedString("xy"))
  }

  test("required 3 vowels") {
    assert(NiceStringDeterminer.ContainsThreeRequiredVowels("aei"))
    assert(NiceStringDeterminer.ContainsThreeRequiredVowels("iou"))
    assert(NiceStringDeterminer.ContainsThreeRequiredVowels("aaa"))

    assert(! NiceStringDeterminer.ContainsThreeRequiredVowels(""))
    assert(! NiceStringDeterminer.ContainsThreeRequiredVowels("aba")) // only 2 vowels
    assert(! NiceStringDeterminer.ContainsThreeRequiredVowels("by"))
  }

  test("required double letters") {
    assert(NiceStringDeterminer.ContainsDoubleLetter("aa"))
    assert(NiceStringDeterminer.ContainsDoubleLetter("aaa"))

    assert(! NiceStringDeterminer.ContainsDoubleLetter("ab"))
    assert(! NiceStringDeterminer.ContainsDoubleLetter("ababababababababa"))
  }

  test("overall nicety of a string (previous 3 rules together)") {
    assert(NiceStringDeterminer.IsNice("aaa"))
    assert(NiceStringDeterminer.IsNice("ugknbfddgicrmopn"))

    assert(! NiceStringDeterminer.IsNice("jchzalrnumimnmhp"))
    assert(! NiceStringDeterminer.IsNice("haegwjzuvuyypxyu"))
    assert(! NiceStringDeterminer.IsNice("dvszwmarrgswjxmb"))
  }
}

class Day5Part2Tests extends FunSuite {
  test("pair of letters without overlapping") {
    val hasPair = NiceStringDeterminer2.ContainsPairOfLettersWithoutOverlapping _
    assert(hasPair("xyxy"))
    assert(hasPair("aabcdefgaa"))

    assert(! hasPair("aaa"))
  }

  test("letter repeating with one letter in between") {
    assert(NiceStringDeterminer2.ContainsLetterRepeatingWithOneLetterInBetween("xyx"))
    assert(NiceStringDeterminer2.ContainsLetterRepeatingWithOneLetterInBetween("abcdefeghi"))
    assert(NiceStringDeterminer2.ContainsLetterRepeatingWithOneLetterInBetween("aaa"))
  }

  test("niceness (both rules together)") {
    assert(NiceStringDeterminer2.IsNice("qjhvhtzxzqqjkmpb"))
    assert(NiceStringDeterminer2.IsNice("xxyxx"))

    assert(! NiceStringDeterminer2.IsNice("uurcxstgmygtbstg"))
    assert(! NiceStringDeterminer2.IsNice("ieodomkazucvgmuy"))
  }
}

class Day5SolutionTests extends BaseSolutionTests {
  test("how many strings in the input are nice?") {
    dontRunSolutionAutomatically {
      Day5Solution.HowManyStringsAreNice()
      // res326: Int = 258
    }
  }

  test("niceness part 2") {
    dontRunSolutionAutomatically {
      Day5Solution.HowManyStringsAreNicePart2()
      // res337: Int = 53
    }
  }
}
