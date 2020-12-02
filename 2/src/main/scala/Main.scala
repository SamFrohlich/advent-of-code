import scala.io.Source
import parsley._

// A very Haskell solution:
object Main:
  def main(args: Array[String]): Unit =

    // Get input
    val bufferedSource = Source.fromFile("passwords.txt")
    val input = bufferedSource.getLines.toList
    bufferedSource.close

    // Parser
    val nat: Parsley[Int] = TokenParser(LanguageDef.plain).natural
    val pass: Parsley[String] = Combinator.some(Char.letter).map(_.mkString(""))
    val parseScheme: Parsley[PasswordScheme] =
      (nat <* Char.char('-')).map(mkPS)
      <*> (nat <* Char.char(' '))
      <*> (Char.letter <* Char.string(": "))
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
