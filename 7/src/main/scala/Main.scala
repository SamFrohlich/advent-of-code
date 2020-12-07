import scala.io.Source
import parsley._
import parsley.Parsley._
import parsley.Combinator.{some, sepEndBy1}
import parsley.Char.{letter, char}
import parsley.Implicits.{charLift, stringLift}
import scala.language.implicitConversions

object Main:
  def main(args: Array[String]): Unit =

    val bufferedSource = Source.fromFile("rules.txt")
    val rs = bufferedSource.mkString
    bufferedSource.close
    // println(List.concat(List('a'), List('b')))
    // println(runParser(bag <* gobble, "dark olive bags contain "))
    // println(runParser(content, "4 clear orange bags."))
    // println(runParser(contents, "4 clear orange bags."))
    //println(runParser(contents, "2 dotted green bags, 1 dull brown bag, 3 striped tomato bags, 5 muted blue bags."))
    //println(runParser(contents, "2 dotted green bags, 1 dull brown bag, 3 striped tomato bags, 5 muted blue bags."))
    //println(runParser(rule, "dull blue bags contain 2 dotted green bags, 1 dull brown bag, 3 striped tomato bags, 5 muted blue bags."))
    //val test = "dull blue bags contain 2 dotted green bags, 1 dull brown bag, 3 striped tomato bags, 5 muted blue bags.\ndotted cyan bags contain 2 faded lavender bags, 1 drab fuchsia bag, 5 bright blue bags."
    //println(runParser(rules, test))
    //println(runParser(rule, "striped tomato bags contain no other bags."))
    val ruleMap: Map[String, List[(Int,String)]] = runParser(rules, rs).toOption.get.toMap
    //println(ruleMap)
    //println(ruleMap.map((_,bs) => bs.map(_._2).contains("shinygold")))
    //println(ruleMap.filter(f).keys)
    //f("dullblue", List((1,"shinygold"),(2, "wavyred")))
    val ex = "mirrored plum bags contain 5 bright bronze bags, 5 shiny gold bags, 5 dark plum bags."
    val parsedEx: Map[String, List[(Int,String)]] = runParser(rules, ex).toOption.get.toMap
    println(parsedEx)
    println((parsedEx.head))
    println(f(parsedEx.head))
    println(parsedEx.filter(f))

def f(x: (String, List[(Int, String)])): Boolean =
  //x._2.map(_._2).contains(List('s', 'h', 'i', 'n', 'y', 'g', 'o', 'l', 'd'))
  val xs: List[String] = x._2.map(_._2)
  println(xs)
  xs.contains("shinygold")

// parser:
val bag: Parsley[String] =
  lift2[List[Char], List[Char], List[Char]]
    ( _ ::: _
    , some(letter) <* ' '
    , some(letter))
    .map(_.toString)
val gobble = " bags contain "
val content = lift2((_:Int,_:String), TokenParser(LanguageDef.plain).natural <* ' ', bag <*  (" bags" <\> " bag"))
val end = "no other bags" #> List()
val contents = (sepEndBy1(content, ", ") ) <* '.'
val key = bag <* gobble
val value = contents <\> end
val rule = lift2((_:String,_:List[(Int,String)]), key, value)
val rules = sepEndBy1(rule, '\n')