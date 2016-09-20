import org.scalatest.FunSuite
import scala.concurrent.{ExecutionContext, Future}

class Day19Tests extends FunSuite {
  test("get molecules in input") {
    assert(TokenParser.tokenize("HOHO", Set("H", "O")) ==
      Vector(
        KnownMolecule(0, "H"),
        KnownMolecule(1, "O"),
        KnownMolecule(2, "H"),
        KnownMolecule(3, "O")
      ))

    assert(TokenParser.tokenize("HoHO", Set("H", "O", "Ho")) ==
      Vector(
        KnownMolecule(0, "Ho"),
        KnownMolecule(2, "H"),
        KnownMolecule(3, "O")
      ))

    assert(TokenParser.tokenize("HooHo", Set("H", "Ho", "Hoo")) ==
      Vector(KnownMolecule(0, "Hoo"), KnownMolecule(3, "Ho")))

    // can get molecules in input that contains garbage
    assert(TokenParser.tokenize("...HO...HO...", Set("H", "O")) ==
      Vector(
        UnknownMolecule(0, "..."),
        KnownMolecule(3, "H"),
        KnownMolecule(4, "O"),
        UnknownMolecule(5, "..."),
        KnownMolecule(8, "H"),
        KnownMolecule(9, "O")
      ))
  }

  test("replace molecules") {
    val replacements = Map("H" -> List("HO", "OH"), "O" -> List("HH"))
    val molecules = TokenParser.tokenize("HOH", replacements.map(_._1).toSet)

    Day19Solution.replacements(molecules, replacements)
    assert(Day19Solution.replacements(molecules, replacements) == Set("HOOH", "HOHO", "OHOH", "HHHH"))
  }

  test("replace molecules with varying lengths") {
    val replacements = Map("Ho" -> List("HO"), "O" -> List("hh"))
    val molecules = TokenParser.tokenize("HoHO", replacements.map(_._1).toSet)

    val result = Day19Solution.replacements(molecules, replacements)
    assert(result == Set("HOHO", "HoHhh"))
  }
}

class Day19SolutionTests extends BaseSolutionTests {
  test("tokenize all input") {
    val tokens = TokenParser.tokenize(
      Day19Solution.medicineMolecule,
      Day19Solution.replacementMolecules.keys.toSet
    )
    tokens.size
  }

  test("solve part1") {
    dontRunSolutionAutomatically {
      val molecules = Day19Solution.solvePart1PossibleMolecules()
      molecules.size
      // res56: Int = 518
    }
  }
}
