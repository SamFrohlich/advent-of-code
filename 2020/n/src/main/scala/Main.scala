import scala.io.Source

object Main:
  def main(args: Array[String]): Unit =

    val bufferedSource = Source.fromFile("TODO.txt")
    val todo = bufferedSource.getLines.toList
    bufferedSource.close
    println(todo)



