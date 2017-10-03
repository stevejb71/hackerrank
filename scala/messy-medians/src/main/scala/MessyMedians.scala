import scala.collection.mutable

class MedianSet {
  private val values = new mutable.ArrayBuffer[Int]()

  def add(value: Int): Unit = {
    def go(i: Int): Unit = {
      if(i == values.length || values(i) >= value) {
        values.insert(i, value)
      } else {
        go(i + 1)
      }
    }
    go(0)
  }

  def median = {
    val middle = values.length / 2
    if(values.length % 2 == 0) {
      values(middle - 1)
    } else {
      values(middle)
    }
  }
}

object MessyMedians {

}