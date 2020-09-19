import java.io.PrintWriter

import scala.annotation.tailrec

case class TrieNode(var value: Option[String], children: Array[TrieNode]) {
  def insert(value: String): Unit = Trie.insert(this, value)
}

object TrieNode {
  def apply(value: Option[String] = None): TrieNode = new TrieNode(value, Array.ofDim(10))
}

object Trie {
  def insert(root: TrieNode, value: String): Unit = {
    @tailrec
    def go(n: TrieNode, index: Int): TrieNode = {
      if (index == value.length) return n
      val ch = value(index) - '0'
      val child = n.children(ch)
      val nextTrie = if (child == null) {
        val newChild = TrieNode()
        n.children(ch) = newChild
        newChild
      } else {
        child
      }
      go(nextTrie, index + 1)
    }

    val n = go(root, 0)
    n.value = Some(value)
  }

  def countPrefixes(root: TrieNode, value: String): Int = {
    @tailrec
    def go(n: TrieNode, index: Int, count: Int): Int = {
      if (index == value.length) return count
      val ch = value(index) - '0'
      val child = n.children(ch)
      if (child == null) {
        count
      } else {
        go(child, index + 1, if (child.value.isDefined) {
          count + 1
        } else {
          count
        })
      }
    }

    go(root, 0, 0)
  }
}

object Solution {
  def twoTwo(s: String): Int = {
    val maxLength = 242
    var total = 0
    for (i <- 0 until s.length) {
      val ch = s(i)
      if (ch != '0') {
        val sub = s.substring(i, s.length min (i + maxLength))
        total += Trie.countPrefixes(powers, sub)
      }
    }
    total
  }

  val powers: TrieNode = {
    val t = TrieNode()
    var pow: BigInt = 1
    for (_ <- 0 to 800) {
      t.insert(pow.toString)
      pow *= 2
    }
    t
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))
    val t = stdin.readLine.trim.toInt
    for (_ <- 1 to t) {
      val a = stdin.readLine
      val result = twoTwo(a)
      printWriter.println(result)
    }
    printWriter.close()
  }
}
