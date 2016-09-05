import org.scalatest.FunSuite

class Day8Tests extends FunSuite {

  test("count of characters in a serialized string") {
    assert(literalLength("""   ""   """) == 2)
    assert(literalLength("""   "abc"   """) == 5)
    assert(literalLength("""   "aaa\"aaa"   """) == 10)
    assert(literalLength("""   "\x27"   """) == 6)
  }

  test("count of characters in memory") {
    assert(memoryLength("""   "ad\\b"   """) == 4)

    assert(memoryLength("""   ""   """) == 0)
    assert(memoryLength("""   "abc"   """) == 3)
    assert(memoryLength("""   "aaa\"aaa"   """) == 7)
    assert(memoryLength("""   "\x27"   """) == 1)
  }

  test("approximation of characters in memory") {
    assert(LengthCounter.charactersInMemory(trim(""" "a" """)) == "a")

    assert(LengthCounter.charactersInMemory(trim(""" "\\" """)) == ".")
    assert(LengthCounter.charactersInMemory(trim(""" "\\\\" """)) == "..")

    assert(LengthCounter.charactersInMemory(trim(""" "\"" """)) == ".")
    assert(LengthCounter.charactersInMemory(trim(""" "\"\"" """)) == "..")

    assert(LengthCounter.charactersInMemory(trim(""" "\x00" """)) == ".")
    assert(LengthCounter.charactersInMemory(trim(""" "\x00\x00" """)) == "..")
  }

  def trim(s: String): String = {
    s.split("\n").map(_.trim()).mkString
  }

  def literalLength(s: String): Int = {
    LengthCounter.countOfSerializedCharacters(trim(s))
  }

  def memoryLength(s: String): Int = {
    LengthCounter.countOfCharactersInMemory(trim(s))
  }
}

class Day8SolutionTests extends BaseSolutionTests {
  test("solve part 1") {
    dontRunSolutionAutomatically {
      Day8Solution.Solve()
      // res13: Int = 1371
    }
  }
}
