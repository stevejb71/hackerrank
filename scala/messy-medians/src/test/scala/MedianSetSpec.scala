import org.specs2.mutable.Specification

class MedianSetSpec extends Specification {
  "set with one element has median of that element" >> {
    val s = new MedianSet
    s.add(7)
    s.median must_== 7
  }

  "set with two elements has median of the smaller element when the smaller is inserted first" >> {
    val s = new MedianSet
    s.add(3)
    s.add(7)
    s.median must_== 3
  }

  "set with two elements has median of the smaller element when the smaller is inserted second" >> {
    val s = new MedianSet
    s.add(7)
    s.add(3)
    s.median must_== 3
  }

  "set with three elements has median of the middle element" >> {
    val s = new MedianSet
    s.add(7)
    s.add(3)
    s.add(4)
    s.median must_== 4
  }

  "set with four elements has median of the second element" >> {
    val s = new MedianSet
    s.add(7)
    s.add(3)
    s.add(5)
    s.add(4)
    s.median must_== 4
  }
}
