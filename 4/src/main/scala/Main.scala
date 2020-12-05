import scala.io.Source
import parsley._
import parsley.Parsley._
import parsley.Char.{alphaNum, char, digit, letter, newline, hexDigit}
import parsley.Combinator.{option, some, sepBy1, sepEndBy1, repeat}
import parsley.Implicits.{charLift, stringLift}
import scala.language.implicitConversions

// I really should have just used a key value map
object Main:
  def main(args: Array[String]): Unit =

    // Get input
    val bufferedSource = Source.fromFile("passports.txt")
    val input: String = bufferedSource.mkString
    bufferedSource.close

    //parse
    val p: Parsley[List[List[Option[Passport]]]] = sepEndBy1(passDeets, "\n")
    val passports = runParser(p, input)
    println(passports)
    //println(passports.map(x => (x,valid(x))))
    //println(passports.filter(valid).length)

def valid(ps: List[Passport]): Boolean =
  ps.distinctBy(toChar).filter(x => !(compareP(x,Country("1")))).length == 7

def toChar (p: Passport): String = p match
  case Birth(_) => "Birth"
  case Issue(_) => "Issue"
  case Expire(_) => "Expire"
  case Height(_) => "Height"
  case Hair(_) => "Hair"
  case Eye(_) => "Eye"
  case Pass(_) => "Pass"
  case Country(_) => "Country"

def compareP (p1: Passport, p2: Passport): Boolean = (p1,p2) match
  case (Birth(_), Birth(_)) => true
  case (Issue(_), Issue(_)) => true
  case (Expire(_), Expire(_)) => true
  case (Height(_), Height(_)) => true
  case (Hair(_), Hair(_)) => true
  case (Eye(_), Eye(_)) => true
  case (Pass(_), Pass(_)) => true
  case (Country(_), Country(_)) => true
  case _ => false

//AST

trait Passport
case class Birth(year: Int)     extends Passport
case class Issue(year: Int)     extends Passport
case class Expire(year: Int)    extends Passport
case class Height(height: Int)  extends Passport
case class Hair(colour: String) extends Passport
case class Eye(colour: String)  extends Passport
case class Pass(id: String)     extends Passport
case class Country(id: String)  extends Passport

// case parsers
val birth: Parsley[Birth] = ("byr" *> ':' *> nat).map(Birth)
val issue: Parsley[Issue] = ("iyr" *> ':' *> nat).map(Issue)
val expire: Parsley[Expire] = ("eyr" *> ':' *> nat).map(Expire)
val height: Parsley[Height] = ("hgt" *> ':' *> nat <* ("cm" <\> "in")).map(Height)
val hair: Parsley[Hair] = ("hcl" *> ':' *> hex).map(Hair)
val eye: Parsley[Eye] = ("ecl" *> ':' *> eyeColours).map(Eye)
val pass: Parsley[Pass] = ("pid" *> ':' *> id).map(Pass)
val country: Parsley[Country] = ("cid" *> ':' *> id).map(Country)

// helper parsers
val eyeColours: Parsley[String] = "amb" <\> "blu" <\> "brn" <\> "gry" <\> "grn" <\> "hzl" <\> "oth"
val nat: Parsley[Int] = TokenParser(LanguageDef.plain).natural
val hex: Parsley[String] = ('#' *> repeat(6, hexDigit)).map(_.mkString(""))
val word: Parsley[String] = some(letter).map(_.mkString(""))
val id: Parsley[String] = (many('0') *> repeat(9, digit)).map(_.mkString(""))

// combined parser
val passDeet : Parsley[Passport] = birth <\> issue <\> expire <\> height <\> hair
                                 <\> eye <\> pass <\> country

val passDeets : Parsley[List[Option[Passport]]] = sepEndBy1(option(passDeet), ('\n' <\> ' '))