// --- Day 12: JSAbacusFramework.io ---
//
// Santa's Accounting-Elves need help balancing the books after a recent order.
// Unfortunately, their accounting software uses a peculiar storage format.
// That's where you come in.
//
// They have a JSON document which contains a variety of things: arrays
// ([1,2,3]), objects ({"a":1, "b":2}), numbers, and strings. Your first job is
// to simply find all of the numbers throughout the document and add them
// together.
//
// For example:
//
// [1,2,3] and {"a":2,"b":4} both have a sum of 6.
// [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
// {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
// [] and {} both have a sum of 0.
//
// You will not encounter any strings containing numbers.
//
// What is the sum of all numbers in the document?
import org.json4s._
import org.json4s.jackson.JsonMethods

object SantaJsonFilter {
  def withoutNonRedObjects(json: JValue): JValue = {
    json.remove(isRed)
  }

  def isRed(o: JValue): Boolean = {
    o match {
      case JObject(fields) => fields.exists {
        case (key, JString("red")) => true
        case _ => false
      }
      case _ => false
    }
  }
}

object Day12Solution {
  def sum(json: JValue): BigInt = {
    (json \\ classOf[JInt]).sum
  }

  def solveSum(): BigInt = {
    val input = InputReader.ReadInput("Day12.txt")
    val json = JsonMethods.parse(input)

    sum(json)
  }

  def solveNonRedSum(): BigInt = {
    val input = InputReader.ReadInput("Day12.txt")
    val json = JsonMethods.parse(input)

    val withoutRedObjects = SantaJsonFilter.withoutNonRedObjects(json)
    (withoutRedObjects \\ classOf[JInt]).sum
  }
}
