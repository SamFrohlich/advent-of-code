import scala.io.Source
import parsley._
import parsley.Parsley._
import parsley.Char.{letter, space, newline}
import parsley.Combinator.{some, sepEndBy1, sepBy1}
import parsley.Implicits.{charLift, stringLift}
import scala.language.implicitConversions

object Main:
  def main(args: Array[String]): Unit =

    val bufferedSource = Source.fromFile("als.txt")
    val als = bufferedSource.mkString
    bufferedSource.close
    //println(word.runParser("smfnh"))
    //println(ingreds.runParser("smfnh svztk rqqf sfhvsx "))
    //println(alsP.runParser("(contains sesame, wheat)"))
    //println(line.runParser("gbgf szp zrtfglg (contains nuts)"))
    var m = scala.collection.mutable.Map[String, Set[String]]()

    val parsed = lines.runParser(als).toOption.get

    for ((is, as) <- parsed)
      for (a <- as)
        if m.contains(a)
          m += (a -> is.&(m.apply(a)))
        else
          m += (a -> is)

    val im = m.toMap
    val ingredientsThatContainAllergen = im.values.foldRight(Set.empty : Set[String])(_.union(_))
    val allIngreds = parsed.map(_._1).foldRight(Set.empty : Set[String])(_.union(_))
    val allergenFreeIngreds = allIngreds.diff(ingredientsThatContainAllergen)
    //println(allergenFreeIngreds)

    val iOccuraces = parsed.map(_._1.toList).flatten

    var count = 0
    for (i <- iOccuraces)
      if allergenFreeIngreds.contains(i)
        count += 1

    println(im)
    // part 1 ans: println(count)
    // part1 ans:
    println(im.toList.sortBy(_._1))

    // then manually:
    // dairy,HashSet(ppdplc)),
    // eggs,HashSet(gkcplx)),
    // fish,HashSet(ktlh)),
    // nuts,HashSet(msfmt)),
    // sesame,HashSet(dqsbql)),
    // shellfish,HashSet(mvqkdj)),
    // soy,HashSet(ggsz)),
    // wheat,HashSet(hbhsx)))

    // ppdplc,gkcplx,ktlh,msfmt,dqsbql,mvqkdj,ggsz,hbhsx

    // does a food contain an ingred twice?


val word = some(letter).map(_.mkString(""))
val ingreds = sepEndBy1(word, space).map(_.toSet)
val alsP = "(contains " *> sepEndBy1(word, ", ") <* ')'
val line = ingreds <~> alsP
val lines = sepEndBy1(line, newline)

