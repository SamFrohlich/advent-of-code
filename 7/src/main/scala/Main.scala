import scala.io.Source
import parsley._
import parsley.Parsley._
import parsley.Combinator.{some, sepEndBy1, optional}
import parsley.Char.{letter, char}
import parsley.Implicits.{charLift, stringLift}
import scala.language.implicitConversions

object Main:
  def main(args: Array[String]): Unit =

    val bufferedSource = Source.fromFile("rules.txt")
    val rs = bufferedSource.mkString
    bufferedSource.close
    val ruleMap: Map[String, List[(Int,String)]] = runParser(rules, rs).toOption.get.toMap


def f(x: (String, List[(Int, String)]), b: String): Boolean =
  x._2.map(_._2).contains(b)

// parser:
val word = some(letter)
val bag: Parsley[String] =
  lift2[List[Char], List[Char], List[Char]]
    ( _ ::: _
    , word <* ' '
    , word)
    .map(_.mkString)
val gobble = " bags contain "
val content = (TokenParser(LanguageDef.plain).natural <* ' ') <~> (bag <* " bag" <* optional('s'))
val end = "no other bags." #> Nil
val contents = sepEndBy1(content, ", ") <* '.'
val key = bag <* gobble
val value = contents <\> end
val rule = key <~> value
val rules = sepEndBy1(rule, '\n')