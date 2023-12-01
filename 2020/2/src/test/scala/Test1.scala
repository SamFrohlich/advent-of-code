import org.scalatest.Assertions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.implicitConversions
import scala.language.postfixOps

val ex1 = new PasswordScheme(1, 3, 'a', "abcde")
val ex2 = new PasswordScheme(1, 3, 'b', "cdefg")
val ex3 = new PasswordScheme(2, 9, 'c', "ccccccccc")

class Part1 extends AnyFlatSpec with Matchers with Assertions:
  "ex1" should "valid" in:
      ex1.valid should equal(true)
  "ex2" should "not valid" in:
      ex2.valid should equal(false)
  "ex3" should "valid" in:
      ex3.valid should equal(true)

class Part2 extends AnyFlatSpec with Matchers with Assertions:
  "ex1" should "valid" in:
      ex1.valid2 should equal(true)
  "ex2" should "not valid" in:
      ex2.valid2 should equal(false)
  "ex3" should "not valid" in:
      ex3.valid2 should equal(false)
