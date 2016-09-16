import scala.annotation.tailrec

// --- Day 19: Medicine for Rudolph ---
//
// Rudolph the Red-Nosed Reindeer is sick! His nose isn't shining very brightly,
// and he needs medicine.
//
// Red-Nosed Reindeer biology isn't similar to regular reindeer biology; Rudolph
// is going to need custom-made medicine. Unfortunately, Red-Nosed Reindeer
// chemistry isn't similar to regular reindeer chemistry, either.
//
// The North Pole is equipped with a Red-Nosed Reindeer nuclear fusion/fission
// plant, capable of constructing any Red-Nosed Reindeer molecule you need. It
// works by starting with some input molecule and then doing a series of
// replacements, one per step, until it has the right molecule.
//
// However, the machine has to be calibrated before it can be used. Calibration
// involves determining the number of molecules that can be generated in one
// step from a given starting point.
//
// For example, imagine a simpler machine that supports only the following
// replacements:
//
// H => HO
// H => OH
// O => HH
//
// Given the replacements above and starting with HOH, the following molecules
// could be generated:
//
// HOOH (via H => HO on the first H).
// HOHO (via H => HO on the second H).
// OHOH (via H => OH on the first H).
// HOOH (via H => OH on the second H).
// HHHH (via O => HH).
//
// So, in the example above, there are 4 distinct molecules (not five, because
// HOOH appears twice) after one replacement from HOH. Santa's favorite
// molecule, HOHOHO, can become 7 distinct molecules (over nine replacements:
// six from H, and three from O).
//
// The machine replaces without regard for the surrounding characters. For
// example, given the string H2O, the transition H => OO would result in OO2O.
//
// Your puzzle input describes all of the possible replacements and, at the
// bottom, the medicine molecule for which you need to calibrate the machine.
// How many distinct molecules can be created after all the different ways you
// can do one replacement on the medicine molecule?
//

sealed abstract class MoleculeToken(position: Int, text: String)
case class UnknownMolecule(position: Int, text: String) extends MoleculeToken(position, text)
case class KnownMolecule(position: Int, text: String) extends MoleculeToken(position, text)

object TokenParser {
  private case class ParserState(
    position: Int,
    result: List[MoleculeToken],
    unknownPosition: Option[Int]
  )

  /**
   * @example AaBCaaCa -> Aa, B, Caa, Ca
   */
  def tokenize(s: String, molecules: Set[String]) = {
    // try longer ones first
    val tokens = molecules.toList.sortBy(_.length()).reverse

    @tailrec
    def parseTokens(state: ParserState): List[MoleculeToken] = {
      if (state.position == s.length())
        state.result
      else {
        val remaining = s.substring(state.position, s.length())

        tokens.find(remaining.startsWith) match {
          case None => {
            parseTokens(state.copy(
              position = state.position + 1,
              unknownPosition = state.unknownPosition orElse (Some(state.position))
            ))
          }
          case Some(text) => {
            val result = addMatch(text, state)
            parseTokens(state.copy(
              state.position + text.size,
              result = result,
              unknownPosition = None
            ))
          }
        }
      }
    }

    def addMatch(text: String, state: ParserState) = {
      val unknown = state.unknownPosition
        .map(pos => UnknownMolecule(pos, s.substring(pos, state.position)))

      val token = Some(KnownMolecule(state.position, text))

      List(unknown, token)
        .flatten
        .foldLeft(state.result)((result, u) => u :: result)
    }

    parseTokens(ParserState(0, List(), None)).reverse
  }
}

object Day19Solution {
  val targetMolecule = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSi" +
    "RnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaF" +
    "ArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaF" +
    "YCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSi" +
    "ThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnS" +
    "iAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiB" +
    "SiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaP" +
    "BCaPBSiRnFYPBCaFArCaSiAl"

  val replacementMolecules = List(
    "Al" -> "ThF", "Al" -> "ThRnFAr", "B" -> "BCa", "B" -> "TiB",
    "B" -> "TiRnFAr", "Ca" -> "CaCa", "Ca" -> "PB", "Ca" -> "PRnFAr",
    "Ca" -> "SiRnFYFAr", "Ca" -> "SiRnMgAr", "Ca" -> "SiTh", "F" -> "CaF",
    "F" -> "PMg", "F" -> "SiAl", "H" -> "CRnAlAr", "H" -> "CRnFYFYFAr",
    "H" -> "CRnFYMgAr", "H" -> "CRnMgYFAr", "H" -> "HCa", "H" -> "NRnFYFAr",
    "H" -> "NRnMgAr", "H" -> "NTh", "H" -> "OB", "H" -> "ORnFAr", "Mg" -> "BF",
    "Mg" -> "TiMg", "N" -> "CRnFAr", "N" -> "HSi", "O" -> "CRnFYFAr",
    "O" -> "CRnMgAr", "O" -> "HP", "O" -> "NRnFAr", "O" -> "OTi", "P" -> "CaP",
    "P" -> "PTi", "P" -> "SiRnFAr", "Si" -> "CaSi", "Th" -> "ThCa",
    "Ti" -> "BP", "Ti" -> "TiTi", "e" -> "HF", "e" -> "NAl", "e" -> "OMg"
  ).groupBy { case (k, v) => k }
    .mapValues(values => values.map { case (k, v) => v })

  // replacementMolecules.toList.sortBy(_._1)
  // List(
  // (Al,List(ThF, ThRnFAr)),
  // (B,List(BCa, TiB, TiRnFAr)),
  // (Ca,List(CaCa, PB, PRnFAr, SiRnFYFAr, SiRnMgAr, SiTh)),
  // (F,List(CaF, PMg, SiAl)),
  // (H,List(CRnAlAr, CRnFYFYFAr, CRnFYMgAr, CRnMgYFAr, HCa, NRnFYFAr, NRnMgAr, NTh, OB, ORnFAr)),
  // (Mg,List(BF, TiMg)),
  // (N,List(CRnFAr, HSi)),
  // (O,List(CRnFYFAr, CRnMgAr, HP, NRnFAr, OTi)),
  // (P,List(CaP, PTi, SiRnFAr)),
  // (Si,List(CaSi)),
  // (Th,List(ThCa)),
  // (Ti,List(BP, TiTi)),
  // (e,List(HF, NAl, OMg)))

  /** for one step of replacing */
  def replacements(molecules: List[MoleculeToken], transformations: Map[String, List[String]]) = {
    ???
    // molecules.map(m => transformations(m.text))
  }
}
