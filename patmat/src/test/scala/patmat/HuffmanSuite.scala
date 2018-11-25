package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  new TestTrees {
    println(decode(t1, List(0, 1)))
  }

  test("decode some text") {
    new TestTrees {
      assert(decode(t1, List(0, 1)) == List('a', 'b'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val encoded: List[Bit] = encode(t1)("ab".toList)
      val decoded: List[Char] = decode(t1, encoded)
      assert(decoded === "ab".toList)
    }
  }

  test("decode and encode of a longer text should be identity") {
    val testText = List('t', 'h', 'i', 's', 'i', 's', 'a', 't', 'e', 's', 't', 't', 'e', 'x', 't')
    val encoded = encode(frenchCode)(testText)
    val decoded = decode(frenchCode, encoded)
    assert(decoded === testText)
  }

  test("converted french code tree should match expected output") {
    new TestTrees {
      val converted: CodeTable = convert(t2)
      val t2Table: CodeTable =
        List(
          ('a', List(0, 0)),
          ('b', List(1, 0)),
          ('d', List(1))
        )
      assert(converted == t2Table)
    }
  }

}
