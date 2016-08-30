import org.scalatest.FunSuite

class Day2Tests extends FunSuite {
  test("amount of slack paper") {
    val paperFor = WrappingCalculator.amountOfSlackWrapping _
    assert(paperFor(Present(1, 2, 3)) == 2)
    assert(paperFor(Present(2, 3, 4)) == 6)
    assert(paperFor(Present(1, 1, 10)) == 1)
  }

  test("amount of wrapping") {
    val wrappingFor = WrappingCalculator.amountOfWrapping _
    assert(wrappingFor(Present(2, 3, 4)) == 58)
    assert(wrappingFor(Present(1, 1, 10)) == 43)
  }

  test("amount of ribbon for the bow") {
    assert(RibbonCalculator.RibbonForBow(Present(2, 3, 4)) == 24)
    assert(RibbonCalculator.RibbonForBow(Present(1, 1, 10)) == 10)
  }

  test("amount of ribbon for the entire present") {
    assert(RibbonCalculator.AmountOfRibbon(Present(2, 3, 4)) == 34)
    assert(RibbonCalculator.AmountOfRibbon(Present(1, 1, 10)) == 14)
  }
}

class Day2SolutionTests extends BaseSolutionTests {
  test("part 1: amount of wrapping for presents") {
    dontRunSolutionAutomatically {
      Day2Solution.TotalAmountOfWrapping()
      // res178: Int = 1598415
    }
  }

  test("part 2: amount of ribbon for presents") {
    dontRunSolutionAutomatically {
      Day2Solution.TotalAmountOfRibbon()
      // res179: Int = 3812909
    }
  }
}
