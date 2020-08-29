import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MatrixLayerRotationTest extends AnyFlatSpec with Matchers {
  it should "work for sample input 1" in {
    val matrix = Array(1, 5, 9, 13).map(n => (n to n + 3).toArray)
    Solution.matrixRotation(matrix, 4, 4, 2)
    val expected = Vector(
      Vector(3, 4, 8, 12),
      Vector(2, 11, 10, 16),
      Vector(1, 7, 6, 15),
      Vector(5, 9, 13, 14),
    )
    val vectorMatrix = matrix.map(_.toVector).toVector

    vectorMatrix shouldBe expected
  }

  it should "work for sample input 2" in {
    val matrix = Array(1, 7, 13, 19, 25).map(n => (n to n + 3).toArray)
    Solution.matrixRotation(matrix, 5, 4, 7)
    val expected = Vector(
      Vector(28, 27, 26, 25),
      Vector(22, 9, 15, 19),
      Vector(16, 8, 21, 13),
      Vector(10, 14, 20, 7),
      Vector(4, 3, 2, 1),
    )
    val vectorMatrix = matrix.map(_.toVector).toVector

    vectorMatrix shouldBe expected
  }

  it should "work for sample input 3" in {
    val matrix = Array(Array(1, 1), Array(1, 1))
    Solution.matrixRotation(matrix, 2, 2, 3)
    val expected = Vector(
      Vector(1 ,1),
      Vector(1 ,1),
    )
    val vectorMatrix = matrix.map(_.toVector).toVector

    vectorMatrix shouldBe expected
  }

  it should "work for test input 1" in {
    val matrix = Array(1, 5, 9, 13).map(n => (n to n + 3).toArray)
    Solution.matrixRotation(matrix, 4, 4, 1)
    val expected = Vector(
      Vector(2, 3, 4, 8),
      Vector(1, 7, 11, 12),
      Vector(5, 6, 10, 16),
      Vector(9, 13, 14, 15),
    )
    val vectorMatrix = matrix.map(_.toVector).toVector

    vectorMatrix shouldBe expected
  }
}
