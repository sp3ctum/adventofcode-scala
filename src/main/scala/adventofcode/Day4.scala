import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

// --- Day 4: The Ideal Stocking Stuffer ---

// Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
// gifts for all the economically forward-thinking little girls and boys.

// To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
// least five zeroes. The input to the MD5 hash is some secret key (your puzzle
// input, given below) followed by a number in decimal. To mine AdventCoins, you
// must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
// that produces such a hash.

// For example:

// If your secret key is abcdef, the answer is 609043, because the MD5 hash of
// abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest
// such number to do so.
//
// If your secret key is pqrstuv, the lowest number it combines with to make an
// MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of
// pqrstuv1048970 looks like 000006136ef....
//
// Your puzzle input is iwrupvqb.

object MD5 {
  // I copied this implementation from the user "mo" at:
  // https://github.com/mo/advent-of-code-2015/blob/master/scala/src/main/scala/Day04.scala
  def md5(s: String): Array[Byte] =
    MessageDigest.getInstance("MD5").digest(s.getBytes)

  def md5AsString(keyToHash: String): String = {
    MD5.byteArrayToString(MD5.md5(keyToHash))
  }

  private def byteArrayToString(b: Array[Byte]): String =
    DatatypeConverter.printHexBinary(b)
}

object Day4Solution {
  def Input: String = "iwrupvqb"
  def CreateHashForSecretKey(secret: String = Input): Int = {

    // limit the computation until I know how fast it takes for a small set
    val numberGroups = Iterator.from(1).grouped(1000)

    val solvedHashes = numberGroups.flatMap {group =>
      group.par.filter {number =>
        val key = secret + number
        val hash = MD5.md5(key)
        StartsWithZeroes(5, hash)
      }
    }

    solvedHashes.next()
  }

  def StartsWithZeroes(zeroCount: Int, number: Array[Byte]): Boolean = {
    require(zeroCount >= 2)

    // algorithm by the user "mo" at
    // https://github.com/mo/advent-of-code-2015/blob/master/scala/src/main/scala/Day04.scala
    //
    // explanation by me

    // example input for the md5 of "abcdef609043":
    // number: Array[Byte] = Array(0, 0, 1, -37, -65, -93, -91, -56, 58, 45, 80, 100, 41, -57, -80, 14)

    val hasLeadingZero = number(0) == 0

    // a hexadecimal number, say 0xA, contains 4 bits.
    // hexadecimal notation 0x0A contains these bits:
    // 0000 1010
    // Separate both bits as separate numbers.
    val firstBits = number.slice(0, 1 + zeroCount / 2)
      .flatMap(byte => Seq(byte >> 4, byte << 4))
      .slice(0, zeroCount)

    hasLeadingZero && firstBits.forall(_ == 0)
  }
}
