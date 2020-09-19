import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MutableDigitTrieTest extends AnyFlatSpec with Matchers {
  "insert" should "be able to insert an empty string" in {
    val trie = TrieNode()
    Trie.insert(trie, "")
    trie.value shouldBe Some("")
  }

  "insert" should "be able to insert non-empty digit string" in {
    val trie = TrieNode()
    Trie.insert(trie, "256")
    trie.children(2).children(5).children(6).value shouldBe Some("256")
  }

  "insert" should "be able to insert two strings which overlap" in {
    val trie = TrieNode()
    Trie.insert(trie, "256")
    Trie.insert(trie, "2561")
    trie.children(2).children(5).children(6).value shouldBe Some("256")
    trie.children(2).children(5).children(6).children(1).value shouldBe Some("2561")
  }

  "countPrefixes" should "work when there are multiple prefixes" in {
    val trie = TrieNode()
    Trie.insert(trie, "256")
    Trie.insert(trie, "25")
    Trie.insert(trie, "287")
    Trie.insert(trie, "2561")

    Trie.countPrefixes(trie, "25641") shouldBe 2
  }

  "countPrefixes" should "work when there are no prefixes" in {
    val trie = TrieNode()
    Trie.insert(trie, "256")
    Trie.insert(trie, "25")
    Trie.insert(trie, "287")
    Trie.insert(trie, "2561")

    Trie.countPrefixes(trie, "7256") shouldBe 0
  }
}
