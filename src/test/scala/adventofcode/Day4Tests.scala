import org.scalatest.FunSuite
import java.math.BigInteger

class Day4Tests extends FunSuite {
  test("create some random md5 hash") {
    assert(MD5.md5AsString("test") == "098F6BCD4621D373CADE4E832627B4F6")
  }

  test("create precalculated secret keys") {
    val hash = MD5.md5AsString("abcdef609043")
    assert(hash.startsWith("00000"))
  }

  test("find out whether a number starts with zeroes") {
    def tobyte(n: String): Byte = Integer.parseInt(n, 2).toByte

    assert(Day4Solution.StartsWithZeroes(3, Array(0, 0, tobyte("00001111"))))
    assert(Day4Solution.StartsWithZeroes(4, Array(0, 0, 0, 0, tobyte("11111111"))))
    assert(Day4Solution.StartsWithZeroes(5, Array(0, 0, 0, 0, tobyte("00001111"))))
  }
}

class Day4SolutionTests extends BaseSolutionTests {
  test("first solution: create an md5 hash with five leading 0's") {
    dontRunSolutionAutomatically {
      val key = Day4Solution.CreateHashForSecretKey(Day4Solution.Input)
      // key: Int = 346386
      // took about a second on this lenovo thinkpad p50 with this cpu:

     // *-cpu
     //      description: CPU
     //      product: Intel(R) Core(TM) i7-6820HQ CPU @ 2.70GHz
     //      vendor: Intel Corp.
     //      physical id: 7
     //      bus info: cpu@0
     //      version: Intel(R) Core(TM) i7-6820HQ CPU @ 2.70GHz
     //      serial: None
     //      slot: U3E1
     //      size: 3344MHz
     //      capacity: 4005MHz
     //      width: 64 bits
     //      clock: 100MHz
     //      capabilities: [...]
     //      configuration: cores=4 enabledcores=4 threads=8

      assert(MD5.md5AsString(Day4Solution.Input + key).startsWith("00000"))
    }
  }
}
