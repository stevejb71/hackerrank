import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class SolutionTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
  it should "be all good for easy examples" in {
    forAll(Table(
      ("string","result"),
      ("2222222", 7),
      ("245256", 4),
      ("65536", 1),
      ("023223", 4),
      ("33579", 0),


    )) {(s, result) =>
      Solution.twoTwo(s) shouldBe result
    }
  }

  it should "be fast enough for big example" in {
    val strings = io.Source.fromInputStream(getClass.getResourceAsStream("/big_test.txt")).getLines.drop(1).toVector
    val start = System.nanoTime()
    strings.foreach(s => println(Solution.twoTwo(s)))
    val end = System.nanoTime()
    println((end - start) / 1_000_000_000.0)
  }
}
