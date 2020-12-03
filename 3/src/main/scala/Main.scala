import scala.io.Source

// A very Haskell solution:
object Main:
  def main(args: Array[String]): Unit =

    val bufferedSource = Source.fromFile("map.txt")
    val map = bufferedSource.getLines.toList
    bufferedSource.close

    println(sleigh(3,1,map))

def sleigh(along: Int, down: Int, map: List[String]) : Int =
  var x = 0
  var y = 0
  var trees = 0
  println(map.length-1)
  while (y <= map.length-1)
    val col = (x % 31)
    val row = y
    if map(row)(col) == '#' then trees = trees + 1
    x = x + along
    y = y + down

  trees


