import scala.io.Source
import parsley._
import parsley.Parsley._
import parsley.Combinator.{some, sepEndBy1}
import parsley.Char.{letter}
import parsley.Implicits.{charLift, stringLift}
import scala.language.implicitConversions

object Main:
  def main(args: Array[String]): Unit =

    val bufferedSource = Source.fromFile("customs.txt")
    val customs = bufferedSource.mkString
    bufferedSource.close
    val gs = runParser(groups, customs).toOption.get
    println(count2(gs))

def count(gs : List[List[List[Char]]]): Int =
  gs.map(_.flatten.distinct.length).sum

def count2(gs : List[List[List[Char]]]): Int =
  gs.map(_.foldRight(('a' to 'z').toSet)(_.toSet.intersect(_))
          .size)
    .sum

// parser - for separating people and groups

val person : Parsley[List[Char]] = some(letter)
val group : Parsley[List[List[Char]]] = sepEndBy1(person, '\n')
val groups : Parsley[List[List[List[Char]]]] = sepEndBy1(group, '\n')
