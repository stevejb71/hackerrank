import scala.io._

object Solution extends App {
  val mnr = StdIn.readLine.replaceAll("\\s+$", "").split(" ")
  val rows = mnr(0).toInt
  val cols = mnr(1).toInt
  val rot = mnr(2).toInt
  type Matrix = Array[Array[Int]]
  val matrix: Matrix = Array.ofDim[Int](rows, cols)
  for (i <- 0 until rows) {
    matrix(i) = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)
  }

  matrixRotation(matrix, rows, cols, rot)
  for(row <- matrix) {
    println(row.mkString(" "))
  }

  def matrixRotation(matrix: Matrix, rows: Int, cols: Int, rot: Int): Unit = {
    val numLayers = ((rows - 1) / 2) min ((cols - 1) / 2)
    for(layer <- 0 to numLayers) {
      rotateLayer(matrix, rows, cols, rot, layer)
    }
  }

  def rotateLayer(matrix: Matrix, rows: Int, cols: Int, rot: Int, layer: Int): Unit = {
    val countToShift = (rows - layer * 2 - 1) * 2 + (cols - layer * 2 - 1) * 2
    val buffer = Array.ofDim[Int](countToShift)
    copyIntoBuffer(buffer, matrix, rows, cols, layer)
    copyOutOfBuffer(i => buffer((i + (countToShift - rot % countToShift)) % countToShift), matrix, rows, cols, layer)
  }

  def copyIntoBuffer(buffer: Array[Int], matrix: Matrix, rows: Int, cols: Int, layer: Int) = {
    var r = layer
    var c = layer
    var i = 0
    while (i < rows - 2 * layer - 1) {
      buffer(i) = matrix(r)(c)
      i += 1
      r += 1
    }
    while (i < rows - 2 * layer - 1 + cols - 2 * layer - 1) {
      buffer(i) = matrix(r)(c)
      i += 1
      c += 1
    }
    while (i < rows - 2 * layer - 1 + cols - 2 * layer - 1 + rows - 2 * layer - 1) {
      buffer(i) = matrix(r)(c)
      i += 1
      r -= 1
    }
    while (i < rows - 2 * layer - 1 + cols - 2 * layer - 1 + rows - 2 * layer - 1 + cols - 2 * layer - 1) {
      buffer(i) = matrix(r)(c)
      i += 1
      c -= 1
    }
    buffer
  }

  def copyOutOfBuffer(getValue: Int => Int, matrix: Matrix, rows: Int, cols: Int, layer: Int): Unit = {
    var r = layer
    var c = layer
    var i = 0
    while (i < rows - 2 * layer - 1) {
      matrix(r)(c) = getValue(i)
      i += 1
      r += 1
    }
    while (i < rows - 2 * layer - 1 + cols - 2 * layer - 1) {
      matrix(r)(c) = getValue(i)
      i += 1
      c += 1
    }
    while (i < rows - 2 * layer - 1 + cols - 2 * layer - 1 + rows - 2 * layer - 1) {
      matrix(r)(c) = getValue(i)
      i += 1
      r -= 1
    }
    while (i < rows - 2 * layer - 1 + cols - 2 * layer - 1 + rows - 2 * layer - 1 + cols - 2 * layer - 1) {
      matrix(r)(c) = getValue(i)
      i += 1
      c -= 1
    }
  }
}
