import org.scalatest.FunSuite

class Day19Tests extends FunSuite {
  test("get molecules in input") {
    assert(TokenParser.tokenize("HOHO", Set("H", "O")) ==
      List(
        KnownMolecule(0, "H"),
        KnownMolecule(1, "O"),
        KnownMolecule(2, "H"),
        KnownMolecule(3, "O")
      ))

    assert(TokenParser.tokenize("HoHO", Set("H", "O", "Ho")) ==
      List(
        KnownMolecule(0, "Ho"),
        KnownMolecule(2, "H"),
        KnownMolecule(3, "O")
      ))

    assert(TokenParser.tokenize("HooHo", Set("H", "Ho", "Hoo")) ==
      List(KnownMolecule(0, "Hoo"), KnownMolecule(3, "Ho")))

    // can get molecules in input that contains garbage
    assert(TokenParser.tokenize("...HO...HO...", Set("H", "O")) ==
      List(
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
    // res13: scala.collection.immutable.Set[List[String]] = Set(List(HO, OH), List(HH))

    assert(Day19Solution.replacements(molecules, replacements) eq Set("HOOH", "HOHO", "OHOH", "HHHH"))
  }
}

class Day19SolutionTests extends BaseSolutionTests {
  test("tokenize all input") {
    TokenParser.tokenize(Day19Solution.targetMolecule, Day19Solution.replacementMolecules.keys.toSet)
  }
}
