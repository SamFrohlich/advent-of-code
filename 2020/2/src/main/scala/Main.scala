import scala.io.Source
import parsley._
import parsley.Char.{char, letter}
import parsley.Combinator.some
import parsley.Implicits.{charLift, stringLift}
import scala.language.implicitConversions

// A very Haskell solution:
object Main:
  def main(args: Array[String]): Unit =

    // Get input
    val bufferedSource = Source.fromFile("passwords.txt")
    val input = bufferedSource.getLines.toList
    bufferedSource.close

    // Parser
    val nat: Parsley[Int] = TokenParser(LanguageDef.plain).natural
    val pass: Parsley[String] = some(letter).map(_.mkString(""))
    val parseScheme: Parsley[PasswordScheme] =
      (nat <* '-').map(mkPS)
      <*> (nat <* ' ')
      <*> (letter <* ": ")
      <*> pass

    // output solution
    println(input.map(runParser(parseScheme, _).toOption.get).count(_.valid))
    println(input.map(runParser(parseScheme, _).toOption.get).count(_.valid2))

// Curried smart constructor or <*> chaining
def mkPS(min : Int) = (max: Int) => (letter: Char) => (password: String) => PasswordScheme(min, max, letter, password)

// Class containing password entry info and methods for deciding validity
case class PasswordScheme (min: Int, max: Int, letter: Char, password: String):
  def valid: Boolean =
    val occurrences = password.count(_ == letter)
    occurrences <= max && occurrences >= min
  def valid2: Boolean =
    (password(min-1) == letter) ^ (password(max-1) == letter)
