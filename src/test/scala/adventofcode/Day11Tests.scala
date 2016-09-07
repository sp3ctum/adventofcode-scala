import org.scalatest.FunSuite
import scala.annotation.tailrec

class Day11Tests extends FunSuite {
  test("increment password") {
    assert(SantaPasswordCreator.incrementPassword("a") == "b")
    assert(SantaPasswordCreator.incrementPassword("z") == "ba")
    assert(SantaPasswordCreator.incrementPassword("aa") == "ab")
    assert(SantaPasswordCreator.incrementPassword("zz") == "baa")
  }

  test("convert number to arbitrary base") {
    assert(BaseConverter.toBase(0, base = 2) == List(0))
    assert(BaseConverter.toBase(1, base = 2) == List(1))
    assert(BaseConverter.toBase(10, base = 2) == List(1, 0, 1, 0))
  }

  test("convert string to number and back") {
    def convertTwice(s: String): String = {
      SantaPasswordCreator.convertToString(
        SantaPasswordCreator.convertToDigits(s).sum,
        padTo = s.length())
    }

    assert(convertTwice("a") == "a")
    assert(convertTwice("b") == "b")
    assert(convertTwice("z") == "z")
    assert(convertTwice("aa") == "aa")
    assert(convertTwice("zz") == "zz")
  }
}

class Day11SolutionTests extends BaseSolutionTests {
}
