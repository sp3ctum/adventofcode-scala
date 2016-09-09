import org.scalatest.FunSuite
import scala.annotation.tailrec

class Day11PasswordIncrementerTests extends FunSuite {
  test("get next password") {
    assert(SantaPasswordIncrementer.nextValidPassword("abcdefgh") == "abcdffaa")

    // I had a bug about this
    assert(SantaPasswordIncrementer.nextValidPassword("abcdffaa") != "abcdffaa")

    // this takes 4 seconds, too much
    // assert(SantaPasswordIncrementer.nextValidPassword("ghijklmn") == "ghjaabcc")
  }
}

class Day11PasswordCreationTests extends FunSuite {
  test("increment password") {
    assert(SantaPasswordCreator.incrementPassword("a") == "b")
    assert(SantaPasswordCreator.incrementPassword("z") == "aa")
    assert(SantaPasswordCreator.incrementPassword("aa") == "ab")
    assert(SantaPasswordCreator.incrementPassword("zz") == "aaa")
    assert(SantaPasswordCreator.incrementPassword("az") == "ba")

    assert(SantaPasswordCreator.incrementPassword("zzz") == "aaaa")
    assert(SantaPasswordCreator.incrementPassword("zaz") == "zba")
  }
}

class Day11SantaPasswordValidationTests extends FunSuite {
  test("three successive characters") {
    assert(SantaPasswordValidation.containsThreeSuccessiveCharactersByCharValue("abc"))
    assert(SantaPasswordValidation.containsThreeSuccessiveCharactersByCharValue("xyz"))
    assert(SantaPasswordValidation.containsThreeSuccessiveCharactersByCharValue("yza"))
    assert(SantaPasswordValidation.containsThreeSuccessiveCharactersByCharValue("zab"))

    assert(SantaPasswordValidation.containsThreeSuccessiveCharactersByCharValue("aaa") == false)
  }

  test("disallowed letters") {
    val allowedCharacters = "esxnjytufamvqbgpchrwkzd"

    assert(SantaPasswordValidation.doesNotContainDisallowedLetters(allowedCharacters))

    assert(SantaPasswordValidation.doesNotContainDisallowedLetters("i")    == false)
    assert(SantaPasswordValidation.doesNotContainDisallowedLetters("o")    == false)
    assert(SantaPasswordValidation.doesNotContainDisallowedLetters("l")    == false)
    assert(SantaPasswordValidation.doesNotContainDisallowedLetters("hei")  == false)
    assert(SantaPasswordValidation.doesNotContainDisallowedLetters("yo")   == false)
    assert(SantaPasswordValidation.doesNotContainDisallowedLetters("tell") == false)
  }

  test("two different pairs") {
    assert(SantaPasswordValidation.containsTwoDifferentPairs("aabb"))
    assert(SantaPasswordValidation.containsTwoDifferentPairs("somethingaabbsomething"))

    // no pairs
    assert(SantaPasswordValidation.containsTwoDifferentPairs("abcdef") == false)
    // same pair twice
    assert(SantaPasswordValidation.containsTwoDifferentPairs("aabcdaa") == false)
    // only one pair
    assert(SantaPasswordValidation.containsTwoDifferentPairs("aabcdef") == false)
  }

  test("validate everything") {
    assert(SantaPasswordValidation.validate("hijklmmn") == false)
    assert(SantaPasswordValidation.validate("abbceffg") == false)
    assert(SantaPasswordValidation.validate("abbcegjk") == false)
  }
}

class Day11SolutionTests extends BaseSolutionTests {
  test("get santa's next password") {
    dontRunSolutionAutomatically {
      Day11Solution.getSantasNextPassword
      // res117: String = hepxxyzz
    }
  }

  test("get santa's second password") {
    dontRunSolutionAutomatically {
      Day11Solution.getSantasSecondPassword

      SantaPasswordCreator.incrementPassword("hepxxyzz")
      // incorrect:
      // res130: String = hepxxzaa

      SantaPasswordIncrementer.nextValidPassword("hepxxyzz")
      // incorrect:
      // res124: String = hepyyzaa
    }
  }
}
