import java.io._
import scala.io._

object Result {
  var s: String = _

  def initialize(s: String) {
    this.s = s
  }

  def answerQuery(l: Int, r: Int): Int = {
    ???
  }
}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))
    val s = StdIn.readLine
    Result.initialize(s)
    val q = StdIn.readLine.trim.toInt
    for (_ <- 1 to q) {
      val firstMultipleInput = StdIn.readLine.replaceAll("\\s+$", "").split(" ")
      val l = firstMultipleInput(0).toInt
      val r = firstMultipleInput(1).toInt
      val result = Result.answerQuery(l, r)
      printWriter.println(result)
    }
    printWriter.close()
  }
}
