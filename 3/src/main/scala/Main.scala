import scala.io.Source
import scala.language.implicitConversions

object Main:
  def main(args: Array[String]): Unit =

    val bufferedSource = Source.fromFile("map.txt")
    val map = bufferedSource.getLines.toList
    bufferedSource.close

    val slope1 = speedySleigh(1,1,map)
    val slope2 = speedySleigh(3,1,map)
    val slope3 = speedySleigh(5,1,map)
    val slope4 = speedySleigh(7,1,map)
    val slope5 = speedySleigh(1,2,map)

    println(slope1 * slope2 * slope3 * slope4 * slope5)

def sleigh(along: Int, down: Int, map: List[String]) : BigInt =
  var x = 0
  var y = 0
  var trees = 0
  while y <= map.length-1 do
    val col = (x % 31)
    val row = y
    if map(row)(col) == '#' then trees = trees + 1
    x += along
    y += down

  trees

// faster (thanks Jamie)
def sleighTailRec(along: Int, down: Int, map: List[String]) : BigInt =
  val n = map.length
  def go(x: Int, y: Int, trees: Int): Int =
    if y > n - 1 then trees
    else go(x + along, y + down, if map(y)(x % 31) == '#' then trees + 1 else trees)
  go(0, 0, 0)

// Even faster using Vector[Vector[Char]] for the map to reduce the cost of
// lookups. (thank you again Jamie!!! <3 )
def speedySleigh(along: Int, down: Int, map: List[String]) : BigInt =
  val n = map.length
  def go(x: Int, y: Int, trees: Int): Int =
    if y > n - 1 then trees
    else go(x + along, y + down, if map.to(Vector).map(_.to(Vector))(y)(x % 31) == '#' then trees + 1 else trees)
  go(0, 0, 0)

