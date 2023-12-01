import org.scalatest.Assertions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.implicitConversions
import scala.language.postfixOps

class Test1 extends AnyFlatSpec with Matchers with Assertions:
  "findSeatID test1" should "calculate correctly" in:
    findSeatID("FBFBBFFRLR") should equal(357)
  "findSeatID test2" should "calculate correctly" in:
    findSeatID("BFFFBBFRRR") should equal(567)
  "findSeatID test3" should "calculate correctly" in:
    findSeatID("FFFBBBFRRR") should equal(119)
  "findSeatID test4" should "calculate correctly" in:
    findSeatID("BBFFBBFRLL") should equal(820)