import scala.io.Source

object Main:
  def main(args: Array[String]): Unit =

    val bufferedSource = Source.fromFile("seats.txt")
    val seats = bufferedSource.getLines.toList
    bufferedSource.close
    println(seats.map(findSeatID)
                . sorted
                . filter((x,y) => x != 1)
                . filter((x,y) => x != 102)
                . groupBy((x,y)=> x)
                . toList
                . map((x,y)=> y)
                . filter(_.length !=8))
    println(88 * 8 + 1)


def findSeatID(ref: String): (Int,Int) =
  val row = converge('F', ref.take(7), 0, 127)
  val col = converge('L', ref.drop(7), 0, 7)
  (row, col)

def converge(lower: Char, code: String, min: Int, max: Int): Int =
  var workingMin = min
  var workingMax = max
  for (c <- code)
    if c == lower
      workingMax = workingMax - ((workingMax - workingMin) / 2) - 1
    else
      workingMin = workingMin + ((workingMax - workingMin) / 2) + 1
  workingMax
