import scala.io.Source

// A very Haskell solution:
object Main:
  def main(args: Array[String]): Unit =

    val bufferedSource = Source.fromFile("input.txt")
    val input = bufferedSource.getLines
                              .toList
                              .map(_.toInt)

    println("Result1:")
    println(process1(input).head)
    println("Result2:")
    println(process2(input).head)

    bufferedSource.close

  def process1(xs: List[Int]) = for
    x <- xs.view // view makes this behave lazily, short circuiting because of the .head at call site
    y <- xs.view
    if x + y == 2020
  yield (x*y)

  def process2(xs: List[Int]) = for
    x <- xs.view
    y <- xs.view
    z <- xs.view
    if x + y + z == 2020
  yield (x*y*z)


