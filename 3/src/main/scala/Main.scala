import scala.io.Source

// A very Haskell solution:
object Main:
  def main(args: Array[String]): Unit =

    val bufferedSource = Source.fromFile("map.txt")
    val map = bufferedSource.getLines.toList
    bufferedSource.close

    val slope1 = sleigh(1,1,map)
    val slope2 = sleigh(3,1,map)
    val slope3 = sleigh(5,1,map)
    val slope4 = sleigh(7,1,map)
    val slope5 = sleigh(1,2,map)

    println(slope1 * slope2 * slope3 * slope4 * slope5)

def sleigh(along: Int, down: Int, map: List[String]) : BigInt =
  var x = 0
  var y = 0
  var trees = 0
  while (y <= map.length-1)
    val col = (x % 31)
    val row = y
    if map(row)(col) == '#' then trees = trees + 1
    x = x + along
    y = y + down

  trees


