import org.scalatest.Assertions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.implicitConversions
import scala.language.postfixOps
import parsley._
import parsley.Success

class ParserTest extends AnyFlatSpec with Matchers with Assertions:
  "birth" should "parse correctly" in:
      (runParser(birth, "byr:1998").toOption.get) should equal(Birth(1998))
  "issue" should "parse correctly" in:
      (runParser(issue, "iyr:1998").toOption.get) should equal(Issue(1998))
  "expire" should "parse correctly" in:
      (runParser(expire, "eyr:1998").toOption.get) should equal(Expire(1998))
  "height" should "parse correctly" in:
      (runParser(height, "hgt:1998").toOption.get) should equal(Height(1998))
  "hair" should "parse correctly" in:
      (runParser(hair, "hcl:#fffffd").toOption.get) should equal(Hair("fffffd"))
  "eye" should "parse correctly" in:
      (runParser(eye, "ecl:gry").toOption.get) should equal(Eye("gry"))
  "pass" should "parse correctly" in:
      (runParser(pass, "pid:028048884").toOption.get) should equal(Pass("028048884"))
  "country" should "parse correctly" in:
      (runParser(country, "cid:143").toOption.get) should equal(Country("143"))

class FullParserTest extends AnyFlatSpec with Matchers with Assertions:
  "passDeets" should "parse correctly" in:
    (runParser(passDeets, "ecl:gry eyr:2020 hcl:#fffffd").toOption.get) should equal (List(Eye("gry"), Expire(2020), Hair("fffffd")))

