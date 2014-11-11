package dhg.util

import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.mutable.SetBuilder

import org.junit.Assert._
import org.junit.Test

import dhg.util.CollectionUtil._
import dhg.util.TestUtil._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class CollectionUtilTests {

  @Test
  def test_toTuple() {
    val seq = Seq(1, 2)
    val seqT: (Int, Int) = seq.toTuple2
    assertEquals((1, 2), seqT)

    assertException(seq.toTuple3) {
      case e: AssertionError => assertEquals("Cannot convert sequence of length 2 into Tuple3: List(1, 2)", e.getMessage)
    }

    val arr = Array("3", "4", "5", "6")
    val arrT: (String, String, String, String) = arr.toTuple4
    assertEquals(("3", "4", "5", "6"), arrT)

    assertException(arr.toTuple5) {
      case e: AssertionError => assertEquals("Cannot convert array of length 4 into Tuple5: Array(3, 4, 5, 6)", e.getMessage)
    }
  }

  @Test
  def test_prependAppendIterator() {
    val new1: Iterator[Int] = 1 +: Iterator(3, 4, 5)
    assertEqualsIterator(Iterator(1, 3, 4, 5), new1)

    val new2: Iterator[Int] = Iterator(3, 4, 5) :+ 7
    assertEqualsIterator(Iterator(3, 4, 5, 7), new2)

    val new3: Iterator[Int] = 1 +: 2 +: Iterator(3, 4, 5) :+ 6 :+ 7
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5, 6, 7), new3)
  }

  @Test
  def test_counts() {
    val coll1 = Vector('a, 'b, 'c, 'b, 'd, 'a, 'c, 'c, 'b)
    val grouped1: Map[Symbol, Int] = coll1.counts
    assertEquals(Map('a -> 2, 'b -> 3, 'c -> 3, 'd -> 1), grouped1)

    val coll2 = Iterator('a, 'b, 'c, 'b, 'd, 'a, 'c, 'c, 'b)
    val grouped2: Map[Symbol, Int] = coll2.counts
    assertEquals(Map('a -> 2, 'b -> 3, 'c -> 3, 'd -> 1), grouped2)
  }

  @Test
  def test_groupByKey() {
    val coll1 = Vector(1 -> 'a, 2 -> 'b, 3 -> 'c, 2 -> 'a, 2 -> 'b, 3 -> 'd)
    val grouped1: Map[Int, Vector[Symbol]] = coll1.groupByKey
    assertEquals(Map(1 -> Vector('a), 2 -> Vector('b, 'a, 'b), 3 -> Vector('c, 'd)), grouped1)

    val coll2 = Set(1 -> 'a, 2 -> 'b, 3 -> 'c, 2 -> 'a, 3 -> 'd)
    val grouped2: Map[Int, Set[Symbol]] = coll2.groupByKey
    assertEquals(Map(1 -> Set('a), 2 -> Set('b, 'a), 3 -> Set('c, 'd)), grouped2)
  }

  @Test
  def test_ungroup() {
    val grouped1 = Map(1 -> Vector('a), 2 -> Vector('b, 'a, 'b), 3 -> Vector('c, 'd))
    val coll1: Iterator[(Int, Symbol)] = grouped1.ungroup
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 2 -> 'a, 2 -> 'b, 3 -> 'c, 3 -> 'd), coll1)

    val grouped2 = Map(1 -> Set('a), 2 -> Set('b, 'a), 3 -> Set('c, 'd))
    val coll2: Iterator[(Int, Symbol)] = grouped2.ungroup
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 2 -> 'a, 3 -> 'c, 3 -> 'd), coll2)
  }

  @Test
  def test_dropRightWhile() {
    val coll1 = List(1, 2, 3, 4, 5)
    val res1: List[Int] = coll1.dropRightWhile(_ == 0)
    assertEquals(List(1, 2, 3, 4, 5), res1)

    val coll2 = Vector(1, 2, 3, 4, 5)
    val res2: Vector[Int] = coll2.dropRightWhile(_ > 3)
    assertEquals(Vector(1, 2, 3), res2)

    val coll3 = " this  and that "
    val res3: String = coll3.dropRightWhile(_ == ' ')
    assertEquals(" this  and that", res3)
  }

  @Test
  def test_splitAt() {
    val (first1: Iterator[Int], second1: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(3)
    assertEqualsIterator(Iterator(1, 2, 3), first1)
    assertEqualsIterator(Iterator(4, 5), second1)

    val (first2: Iterator[Int], second2: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(3)
    assertException(second2.next) {
      case e: AssertionError => assertEquals("assertion failed: first has NOT YET been read completely", e.getMessage)
    }
    assertEquals(1, first2.next)
    assertEquals(2, first2.next)
    assertEquals(3, first2.next)
    assertException(first2.next) {
      case e: AssertionError => assertEquals("assertion failed: first has already been read completely", e.getMessage)
    }
    assertEquals(4, second2.next)
    assertEquals(5, second2.next)
    assertException(second2.next) {
      case e: AssertionError => assertEquals("assertion failed: second has already been read completely", e.getMessage)
    }

    val (first3: Iterator[Int], second3: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(-1)
    assertEqualsIterator(Iterator(), first3)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), second3)

    val (first4: Iterator[Int], second4: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(0)
    assertEqualsIterator(Iterator(), first4)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), second4)

    val (first5: Iterator[Int], second5: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(5)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), first5)
    assertEqualsIterator(Iterator(), second5)

    val (first6: Iterator[Int], second6: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(6)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), first6)
    assertEqualsIterator(Iterator(), second6)
  }

  @Test
  def test_split() {
    import KeepDelimiter._

    // "12345".split('9')  -->  Array(12345)
    assertEqualsIterator(Iterator(Vector(1, 2, 3, 4, 5)), Vector(1, 2, 3, 4, 5).split(9))
    assertEqualsIterator(Iterator(Vector(1, 2, 3, 4, 5)), Iterator(1, 2, 3, 4, 5).split(9))
    assertEqualsIterator(Iterator(Vector(1, 2, 3, 4, 5)), Vector(1, 2, 3, 4, 5).split(9, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(1, 2, 3, 4, 5)), Iterator(1, 2, 3, 4, 5).split(9, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(1, 2, 3, 4, 5)), Vector(1, 2, 3, 4, 5).split(9, KeepDelimiterAsLast))
    assertEqualsIterator(Iterator(Vector(1, 2, 3, 4, 5)), Iterator(1, 2, 3, 4, 5).split(9, KeepDelimiterAsLast))

    // "12345".split('3')  -->  Array(12, 45)
    assertEqualsIterator(Iterator(List(1, 2), List(4, 5)), List(1, 2, 3, 4, 5).split(3))
    assertEqualsIterator(Iterator(Set(1, 2), Set(4, 5)), Iterator(1, 2, 3, 4, 5).split(3, Set.newBuilder[Int]))
    assertEqualsIterator(Iterator(Set(1, 2), Set(4, 5)), Iterator(1, 2, 3, 4, 5).split(3, Set.newBuilder[Int], DropDelimiter))
    assertEqualsIterator(Iterator(List(1, 2), List(3, 4, 5)), List(1, 2, 3, 4, 5).split(3, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Set(1, 2), Set(3, 4, 5)), Iterator(1, 2, 3, 4, 5).split(3, Set.newBuilder[Int], KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(List(1, 2, 3), List(4, 5)), List(1, 2, 3, 4, 5).split(3, KeepDelimiterAsLast))
    assertEqualsIterator(Iterator(Set(1, 2, 3), Set(4, 5)), Iterator(1, 2, 3, 4, 5).split(3, Set.newBuilder[Int], KeepDelimiterAsLast))

    // "1234225".split('2')  -->  Array(1, 34, "", 5)
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Vector(1, 2, 3, 4, 2, 2, 5).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Iterator(1, 2, 3, 4, 2, 2, 5).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(2, 3, 4), Vector(2), Vector(2, 5)), Vector(1, 2, 3, 4, 2, 2, 5).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(1), Vector(2, 3, 4), Vector(2), Vector(2, 5)), Iterator(1, 2, 3, 4, 2, 2, 5).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(5)), Vector(1, 2, 3, 4, 2, 2, 5).split(2, KeepDelimiterAsLast))
    assertEqualsIterator(Iterator(Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(5)), Iterator(1, 2, 3, 4, 2, 2, 5).split(2, KeepDelimiterAsLast))

    // "212342225".split('2')  -->  Array("", 1, 34, "", "", 5)
    assertEqualsIterator(Iterator(Vector(), Vector(1), Vector(3, 4), Vector(), Vector(), Vector(5)), Vector(2, 1, 2, 3, 4, 2, 2, 2, 5).split(2))
    assertEqualsIterator(Iterator(Vector(), Vector(1), Vector(3, 4), Vector(), Vector(), Vector(5)), Iterator(2, 1, 2, 3, 4, 2, 2, 2, 5).split(2))
    assertEqualsIterator(Iterator(Vector(), Vector(2, 1), Vector(2, 3, 4), Vector(2), Vector(2), Vector(2, 5)), Vector(2, 1, 2, 3, 4, 2, 2, 2, 5).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(), Vector(2, 1), Vector(2, 3, 4), Vector(2), Vector(2), Vector(2, 5)), Iterator(2, 1, 2, 3, 4, 2, 2, 2, 5).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(2), Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(2), Vector(5)), Vector(2, 1, 2, 3, 4, 2, 2, 2, 5).split(2, KeepDelimiterAsLast))
    assertEqualsIterator(Iterator(Vector(2), Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(2), Vector(5)), Iterator(2, 1, 2, 3, 4, 2, 2, 2, 5).split(2, KeepDelimiterAsLast))

    // "221234225".split('2')  -->  Array("", "", 1, 34, "", 5)
    assertEqualsIterator(Iterator(Vector(), Vector(), Vector(1), Vector(3, 4), Vector(), Vector(5)), Vector(2, 2, 1, 2, 3, 4, 2, 2, 5).split(2))
    assertEqualsIterator(Iterator(Vector(), Vector(), Vector(1), Vector(3, 4), Vector(), Vector(5)), Iterator(2, 2, 1, 2, 3, 4, 2, 2, 5).split(2))
    assertEqualsIterator(Iterator(Vector(), Vector(2), Vector(2, 1), Vector(2, 3, 4), Vector(2), Vector(2, 5)), Vector(2, 2, 1, 2, 3, 4, 2, 2, 5).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(), Vector(2), Vector(2, 1), Vector(2, 3, 4), Vector(2), Vector(2, 5)), Iterator(2, 2, 1, 2, 3, 4, 2, 2, 5).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(2), Vector(2), Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(5)), Vector(2, 2, 1, 2, 3, 4, 2, 2, 5).split(2, KeepDelimiterAsLast))
    assertEqualsIterator(Iterator(Vector(2), Vector(2), Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(5)), Iterator(2, 2, 1, 2, 3, 4, 2, 2, 5).split(2, KeepDelimiterAsLast))

    // "12342252".split('2')  -->  Array(1, 34, "", 5)
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Vector(1, 2, 3, 4, 2, 2, 5, 2).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Iterator(1, 2, 3, 4, 2, 2, 5, 2).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(2, 3, 4), Vector(2), Vector(2, 5), Vector(2)), Vector(1, 2, 3, 4, 2, 2, 5, 2).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(1), Vector(2, 3, 4), Vector(2), Vector(2, 5), Vector(2)), Iterator(1, 2, 3, 4, 2, 2, 5, 2).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(5, 2)), Vector(1, 2, 3, 4, 2, 2, 5, 2).split(2, KeepDelimiterAsLast))
    assertEqualsIterator(Iterator(Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(5, 2)), Iterator(1, 2, 3, 4, 2, 2, 5, 2).split(2, KeepDelimiterAsLast))

    // "123422522".split('2')  -->  Array(1, 34, "", 5)
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5), Vector()), Vector(1, 2, 3, 4, 2, 2, 5, 2, 2).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5), Vector()), Iterator(1, 2, 3, 4, 2, 2, 5, 2, 2).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(2, 3, 4), Vector(2), Vector(2, 5), Vector(2), Vector(2)), Vector(1, 2, 3, 4, 2, 2, 5, 2, 2).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(1), Vector(2, 3, 4), Vector(2), Vector(2, 5), Vector(2), Vector(2)), Iterator(1, 2, 3, 4, 2, 2, 5, 2, 2).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(5, 2), Vector(2)), Vector(1, 2, 3, 4, 2, 2, 5, 2, 2).split(2, KeepDelimiterAsLast))
    assertEqualsIterator(Iterator(Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(5, 2), Vector(2)), Iterator(1, 2, 3, 4, 2, 2, 5, 2, 2).split(2, KeepDelimiterAsLast))

    // "1234225222".split('2')  -->  Array(1, 34, "", 5)
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5), Vector(), Vector()), Vector(1, 2, 3, 4, 2, 2, 5, 2, 2, 2).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5), Vector(), Vector()), Iterator(1, 2, 3, 4, 2, 2, 5, 2, 2, 2).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(2, 3, 4), Vector(2), Vector(2, 5), Vector(2), Vector(2), Vector(2)), Vector(1, 2, 3, 4, 2, 2, 5, 2, 2, 2).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(1), Vector(2, 3, 4), Vector(2), Vector(2, 5), Vector(2), Vector(2), Vector(2)), Iterator(1, 2, 3, 4, 2, 2, 5, 2, 2, 2).split(2, KeepDelimiterAsFirst))
    assertEqualsIterator(Iterator(Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(5, 2), Vector(2), Vector(2)), Vector(1, 2, 3, 4, 2, 2, 5, 2, 2, 2).split(2, KeepDelimiterAsLast))
    assertEqualsIterator(Iterator(Vector(1, 2), Vector(3, 4, 2), Vector(2), Vector(5, 2), Vector(2), Vector(2)), Iterator(1, 2, 3, 4, 2, 2, 5, 2, 2, 2).split(2, KeepDelimiterAsLast))
  }

  @Test
  def test_zipSafe() {
    val a = Vector(1, 2, 3)
    val b = List('a, 'b, 'c)

    val res1: Iterator[(Int, Symbol)] = a.iterator zipSafe b
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 3 -> 'c), res1)

    val res2: Vector[(Int, Symbol)] = a zipSafe b
    assertEquals(Vector(1 -> 'a, 2 -> 'b, 3 -> 'c), res2)

    val res3: Iterator[(Int, Symbol)] = (a.iterator, b).zipSafe
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 3 -> 'c), res3)

    val res4: List[(Int, Symbol)] = (a.toList, b).zipSafe
    assertEquals(List(1 -> 'a, 2 -> 'b, 3 -> 'c), res4)

    val c = Vector(1, 2, 3, 4)

    val res5: Iterator[(Symbol, Int)] = b.iterator zipSafe c
    assertEquals(('a, 1), res5.next)
    assertEquals(('b, 2), res5.next)
    assertEquals(('c, 3), res5.next)
    assertException(res5.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.  First ran out.", e.getMessage)
    }

    val res6: Iterator[(Int, Symbol)] = c.iterator zipSafe b
    assertEquals((1, 'a), res6.next)
    assertEquals((2, 'b), res6.next)
    assertEquals((3, 'c), res6.next)
    assertException(res6.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.  Second ran out.", e.getMessage)
    }

    assertException(b zipSafe c) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.  First ran out.", e.getMessage)
    }

    assertException(c zipSafe b) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.  Second ran out.", e.getMessage)
    }

    val res7: Iterator[(Symbol, Int)] = (b.iterator, c).zipSafe
    assertEquals(('a, 1), res7.next)
    assertEquals(('b, 2), res7.next)
    assertEquals(('c, 3), res7.next)
    assertException(res7.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.  First ran out.", e.getMessage)
    }

    val res8: Iterator[(Int, Symbol)] = (c.iterator, b).zipSafe
    assertEquals((1, 'a), res8.next)
    assertEquals((2, 'b), res8.next)
    assertEquals((3, 'c), res8.next)
    assertException(res8.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.  Second ran out.", e.getMessage)
    }

    assertException((b, c).zipSafe) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.  First ran out.", e.getMessage)
    }

    assertException((c, b).zipSafe) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.  Second ran out.", e.getMessage)
    }
  }

  @Test
  def test_unzip4() {
    val v1 = Vector(
      (1, "a", 'a, 'a'),
      (2, "b", 'b, 'b'),
      (3, "c", 'c, 'c')).par
    val (i, s, m, c) = v1.unzip4
    assertEquals(Vector(1, 2, 3).par, i)
    assertEquals(Vector("a", "b", "c").par, s)
    assertEquals(Vector('a, 'b, 'c).par, m)
    assertEquals(Vector('a', 'b', 'c').par, c)
  }

  @Test
  def test_unzip5() {
    val v1 = Vector(
      (1, "a", 'a, 'a', 1.0),
      (2, "b", 'b, 'b', 2.0),
      (3, "c", 'c, 'c', 3.0)).par
    val (i, s, m, c, d) = v1.unzip5
    assertEquals(Vector(1, 2, 3).par, i)
    assertEquals(Vector("a", "b", "c").par, s)
    assertEquals(Vector('a, 'b, 'c).par, m)
    assertEquals(Vector('a', 'b', 'c').par, c)
    assertEquals(Vector(1.0, 2.0, 3.0).par, d)
  }

  @Test
  def test_unzip6() {
    val v1 = Vector(
      (1, "a", 'a, 'a', 1.0, 1l),
      (2, "b", 'b, 'b', 2.0, 2l),
      (3, "c", 'c, 'c', 3.0, 3l)).par
    val (i, s, m, c, d, l) = v1.unzip6
    assertEquals(Vector(1, 2, 3).par, i)
    assertEquals(Vector("a", "b", "c").par, s)
    assertEquals(Vector('a, 'b, 'c).par, m)
    assertEquals(Vector('a', 'b', 'c').par, c)
    assertEquals(Vector(1.0, 2.0, 3.0).par, d)
    assertEquals(Vector(1l, 2l, 3l).par, l)
  }

  @Test
  def test_unzip() {
    val aBuf = Buffer[Int]()
    val bBuf = Buffer[Char]()

    def stuff = Iterator(1 -> 'a', 2 -> 'b', 3 -> 'c').map { case (a, b) => aBuf += a; bBuf += b; (a, b) }

    assertEquals(Buffer(), aBuf)
    assertEquals(Buffer(), bBuf)

    val (a1, b1) = stuff.unzip
    assertEquals(true, a1.hasNext)
    assertEquals(true, b1.hasNext)
    assertEquals(Buffer(), aBuf)
    assertEquals(Buffer(), bBuf)
    assertEquals(1, a1.next())
    assertEquals(true, a1.hasNext)
    assertEquals(true, b1.hasNext)
    assertEquals(Buffer(1), aBuf)
    assertEquals(Buffer('a'), bBuf)
    assertEquals('a', b1.next())
    assertEquals(true, a1.hasNext)
    assertEquals(true, b1.hasNext)
    assertEquals(Buffer(1), aBuf)
    assertEquals(Buffer('a'), bBuf)
    assertEquals('b', b1.next())
    assertEquals(true, a1.hasNext)
    assertEquals(true, b1.hasNext)
    assertEquals(Buffer(1, 2), aBuf)
    assertEquals(Buffer('a', 'b'), bBuf)
    assertEquals(2, a1.next())
    assertEquals(true, a1.hasNext)
    assertEquals(true, b1.hasNext)
    assertEquals(Buffer(1, 2), aBuf)
    assertEquals(Buffer('a', 'b'), bBuf)
    assertEquals(3, a1.next())
    assertEquals(false, a1.hasNext)
    assertEquals(true, b1.hasNext)
    assertEquals(Buffer(1, 2, 3), aBuf)
    assertEquals(Buffer('a', 'b', 'c'), bBuf)
    assertEquals('c', b1.next())
    assertEquals(false, a1.hasNext)
    assertEquals(false, b1.hasNext)
    assertEquals(Buffer(1, 2, 3), aBuf)
    assertEquals(Buffer('a', 'b', 'c'), bBuf)

    aBuf.clear()
    bBuf.clear()

    val (a2, b2) = stuff.unzip
    assertEquals(true, a2.hasNext)
    assertEquals(true, b2.hasNext)
    assertEquals(Buffer(), aBuf)
    assertEquals(Buffer(), bBuf)
    assertEquals(Vector('a', 'b', 'c'), b2.toVector)
    assertEquals(true, a2.hasNext)
    assertEquals(false, b2.hasNext)
    assertEquals(Buffer(1, 2, 3), aBuf)
    assertEquals(Buffer('a', 'b', 'c'), bBuf)
    assertEquals(Vector(1, 2, 3), a2.toVector)
    assertEquals(false, a2.hasNext)
    assertEquals(false, b2.hasNext)
    assertEquals(Buffer(1, 2, 3), aBuf)
    assertEquals(Buffer('a', 'b', 'c'), bBuf)
  }

  @Test
  def test_mapTo() {
    val coll1 = List(1, 2, 3, 4)
    val res1: List[(Int, Int)] = coll1.mapTo(_ + 2)
    assertEquals(List(1 -> 3, 2 -> 4, 3 -> 5, 4 -> 6), res1)

    val coll2 = Set(1, 2, 3, 4)
    val res2: Set[(Int, Int)] = coll2.mapTo(_ + 2)
    assertEquals(Set(1 -> 3, 2 -> 4, 3 -> 5, 4 -> 6), res2)

    val coll3 = Iterator(1, 2, 3, 4)
    val res3: Iterator[(Int, Int)] = coll3.mapTo(_ + 2)
    assertEqualsIterator(Iterator(1 -> 3, 2 -> 4, 3 -> 5, 4 -> 6), res3)
  }

  @Test
  def test_mapToVal() {
    var counter1 = 1
    val coll1 = List('a, 'b, 'c, 'd)
    val res1: List[(Symbol, Int)] = coll1.mapToVal({ counter1 *= 2; counter1 + 3 })
    assertEquals(List('a -> 5, 'b -> 7, 'c -> 11, 'd -> 19), res1)

    var counter2 = 1
    val coll2 = Set('a, 'b, 'c, 'd)
    val res2: Set[(Symbol, Int)] = coll2.mapToVal({ counter2 *= 2; counter2 + 3 })
    assertEquals(Set('a -> 5, 'b -> 7, 'c -> 11, 'd -> 19), res2)

    var counter3 = 1
    val coll3 = Iterator('a, 'b, 'c, 'd)
    val res3: Iterator[(Symbol, Int)] = coll3.mapToVal({ counter3 *= 2; counter3 + 3 })
    assertEqualsIterator(Iterator('a -> 5, 'b -> 7, 'c -> 11, 'd -> 19), res3)
  }

  @Test
  def test_mapKeys() {
    val coll1 = Map(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val res1: Map[Int, Symbol] = coll1.mapKeys(_ + 2)
    assertEquals(Map(3 -> 'a, 4 -> 'b, 5 -> 'c), res1)

    val coll2 = List(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val res2: List[(Int, Symbol)] = coll2.mapKeys(_ + 2)
    assertEquals(List(3 -> 'a, 4 -> 'b, 5 -> 'c), res2)

    var callCount = 0
    val coll3 = Map(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val res3: Map[Int, Symbol] = coll3.mapKeys(i => { callCount += 1; i + 2 })
    assertEquals(Map(3 -> 'a, 4 -> 'b, 5 -> 'c), res3)
    assertEquals(Map(3 -> 'a, 4 -> 'b, 5 -> 'c), res3)
    assertEquals(3, callCount)
  }

  @Test
  def test_mapVals() {
    val coll1 = Map('a -> 1, 'b -> 2, 'c -> 3)
    val res1: Map[Symbol, Int] = coll1.mapVals(_ + 2)
    assertEquals(Map('a -> 3, 'b -> 4, 'c -> 5), res1)

    val coll2 = List('a -> 1, 'b -> 2, 'c -> 3)
    val res2: List[(Symbol, Int)] = coll2.mapVals(_ + 2)
    assertEquals(List('a -> 3, 'b -> 4, 'c -> 5), res2)

    var callCount = 0
    val coll3 = Map('a -> 1, 'b -> 2, 'c -> 3)
    val res3: Map[Symbol, Int] = coll3.mapVals(i => { callCount += 1; i + 2 })
    assertEquals(Map('a -> 3, 'b -> 4, 'c -> 5), res3)
    assertEquals(Map('a -> 3, 'b -> 4, 'c -> 5), res3)
    assertEquals(3, callCount)
  }

  @Test
  def test_mapt() {
    val coll2_1 = Map(('a, 1), ('b, 2), ('c, 3))
    val res2_1: Map[String, Int] = coll2_1.mapt((x, y) => (x + "x", y + 2))
    assertEquals(Map(("'ax", 3), ("'bx", 4), ("'cx", 5)), res2_1)

    val coll2_2 = List(('a, 1), ('b, 2), ('c, 3))
    val res2_2: List[(String, Int)] = coll2_2.mapt((x, y) => (x + "x", y + 2))
    assertEquals(List(("'ax", 3), ("'bx", 4), ("'cx", 5)), res2_2)

    val coll2_3 = Iterator(('a, 1), ('b, 2), ('c, 3))
    val res2_3: Iterator[(String, Int)] = coll2_3.mapt((x, y) => (x + "x", y + 2))
    assertEqualsIterator(Iterator(("'ax", 3), ("'bx", 4), ("'cx", 5)), res2_3)

    val coll3_1 = Set(('a, 1, 5), ('b, 2, 6), ('c, 3, 7))
    val res3_1: Set[(String, Int, Int)] = coll3_1.mapt((x, y, z) => (x + "x", y + 2, z - 1))
    assertEquals(Set(("'ax", 3, 4), ("'bx", 4, 5), ("'cx", 5, 6)), res3_1)

    val coll3_2 = List(('a, 1, 5), ('b, 2, 6), ('c, 3, 7))
    val res3_2: List[(String, Int, Int)] = coll3_2.mapt((x, y, z) => (x + "x", y + 2, z - 1))
    assertEquals(List(("'ax", 3, 4), ("'bx", 4, 5), ("'cx", 5, 6)), res3_2)

    val coll3_3 = Iterator(('a, 1, 5), ('b, 2, 6), ('c, 3, 7))
    val res3_3: Iterator[(String, Int, Int)] = coll3_3.mapt((x, y, z) => (x + "x", y + 2, z - 1))
    assertEqualsIterator(Iterator(("'ax", 3, 4), ("'bx", 4, 5), ("'cx", 5, 6)), res3_3)
  }

  @Test
  def test_foldLeftWhile() {
    val col_7 = (1 to 5)
    val z_7 = List[Int]()
    def p_x_7(x: Int) = x < 3
    def op_7(z: List[Int], x: Int) = z :+ x
    val res_7: List[Int] = col_7.foldLeftWhile(z_7)((z, x) => p_x_7(x))(op_7)
    val exp_7 = List(1, 2)
    assertEquals(exp_7, col_7.takeWhile(p_x_7).foldLeft(z_7)(op_7))
    assertEquals(exp_7, res_7)

    val col_8 = (1 to 5)
    val z_8 = List[Int]()
    def p_z_8(z: List[Int]) = z.size < 3
    def op_8(z: List[Int], x: Int) = z :+ x
    val res_8: List[Int] = col_8.foldLeftWhile(z_8)((z, x) => p_z_8(z))(op_8)
    val exp_8 = List(1, 2, 3)
    assertEquals(exp_8, {
      var z = z_8
      val it = col_8.iterator
      while (p_z_8(z)) {
        val x = it.next
        z = op_8(z, x)
      }
      z
    })
    assertEquals(exp_8, res_8)

    val col_1 = (1 to 5)
    val res_1: Int = col_1.foldLeftWhile(0)((z, x) => z < 5)((z, x) => z + x)
    assertEquals(6, res_1)

    val col_2 = (1 to 5)
    val res_2: Int = col_2.foldLeftWhile(0)((z, x) => x < 3)((z, x) => z + x)
    assertEquals(3, res_2)

    val col_3 = (1 to 5)
    val res_3: Int = col_3.foldLeftWhile(0)((z, x) => z < 1)((z, x) => z + x)
    assertEquals(1, res_3)

    val col_4 = (1 to 5)
    val res_4: Int = col_4.foldLeftWhile(0)((z, x) => x < 1)((z, x) => z + x)
    assertEquals(0, res_4)

    val col_5 = (1 to 5)
    val res_5: Int = col_5.foldLeftWhile(0)((z, x) => z < 0)((z, x) => z + x)
    assertEquals(0, res_5)

    val col_6 = (1 to 5)
    val res_6: Int = col_6.foldLeftWhile(0)((z, x) => x < 0)((z, x) => z + x)
    assertEquals(0, res_6)
  }

  @Test
  def test_avg() {
    val coll1 = List(1, 2, 2, 5)
    val avg1: Double = coll1.avg
    assertEquals(2.5, avg1, 0.0000001)

    val coll2 = Set(1.0f, 1.5f, 2.5f, 5.0f)
    val avg2: Float = coll2.avg
    assertEquals(2.5f, avg2, 0.0000001)
  }

  @Test
  def test_normalize() {
    val coll1 = List(1, 2, 2, 5)
    val avg1: List[Double] = coll1.normalize
    assertEquals(List(0.1, 0.2, 0.2, 0.5), avg1)

    val coll2 = Set(1.0f, 1.5f, 2.5f, 5.0f)
    val avg2: Set[Float] = coll2.normalize
    assertEquals(Set(0.1f, 0.15f, 0.25f, 0.5f), avg2)
  }

  @Test
  def test_normalizeValues() {
    val coll1 = List('a -> 1, 'b -> 2, 'c -> 2, 'd -> 5)
    val avg1: List[(Symbol, Double)] = coll1.normalizeValues
    assertEquals(List('a -> 0.1, 'b -> 0.2, 'c -> 0.2, 'd -> 0.5), avg1)

    val coll2 = Set('a -> 1.0f, 'b -> 1.5f, 'c -> 2.5f, 'd -> 5.0f)
    val avg2: Set[(Symbol, Float)] = coll2.normalizeValues
    assertEquals(Set('a -> 0.1f, 'b -> 0.15f, 'c -> 0.25f, 'd -> 0.5f), avg2)

    val coll3 = Map('a -> 1.0, 'b -> 1.5, 'c -> 2.5, 'd -> 5.0)
    val avg3: Map[Symbol, Double] = coll3.normalizeValues
    assertEquals(Map('a -> 0.1, 'b -> 0.15, 'c -> 0.25, 'd -> 0.5), avg3)
  }

  @Test
  def test_maxByN() {
    val col0 = Vector("a", "three")
    val col1 = Vector("a", "three", "to", "cheese", "bird", "big")
    val col2 = Vector("a", "dog", "i", "to", "as", "big")

    assertEquals(Vector("three", "a"), col0.maxByN(3)(_.length))
    assertEquals(Vector("cheese", "three", "bird"), col1.maxByN(3)(_.length))
    assertEquals(Vector("dog", "big", "to"), col2.maxByN(3)(_.length))

    assertEquals(Set("three", "a"), col0.toSet.maxByN(3)(_.length))
    assertEquals(Set("cheese", "three", "bird"), col1.toSet.maxByN(3)(_.length))
    assertEquals(Set("dog", "big", "as"), col2.toSet.maxByN(3)(_.length))

    assertEquals(Vector("three", "a"), col0.iterator.maxByN(3)(_.length))
    assertEquals(Vector("cheese", "three", "bird"), col1.iterator.maxByN(3)(_.length))
    assertEquals(Vector("dog", "big", "to"), col2.iterator.maxByN(3)(_.length))
  }

  @Test
  def test_minByN() {
    val col0 = Vector("a", "three")
    val col1 = Vector("a", "three", "to", "cheese", "bird", "big")
    val col2 = Vector("a", "dog", "i", "to", "as", "big")

    assertEquals(Vector("a", "three"), col0.minByN(3)(_.length))
    assertEquals(Vector("a", "to", "big"), col1.minByN(3)(_.length))
    assertEquals(Vector("a", "i", "to"), col2.minByN(3)(_.length))

    assertEquals(Set("a", "three"), col0.toSet.minByN(3)(_.length))
    assertEquals(Set("a", "to", "big"), col1.toSet.minByN(3)(_.length))
    assertEquals(Set("a", "i", "as"), col2.toSet.minByN(3)(_.length))

    assertEquals(Vector("a", "three"), col0.iterator.minByN(3)(_.length))
    assertEquals(Vector("a", "to", "big"), col1.iterator.minByN(3)(_.length))
    assertEquals(Vector("a", "i", "to"), col2.iterator.minByN(3)(_.length))
  }

  @Test
  def test_distinctBy() {
    val in1 = Vector("the", "a", "dog", "it", "to", "bag", "door")
    val out1 = Vector("the", "a", "it", "door")
    assertEquals(out1, in1.distinctBy(_.size))
    assertEquals(out1.toList, in1.toList.distinctBy(_.size))
  }

  @Test
  def test_sumBy() {
    assertEquals(21, Vector(1, 3, 5).sumBy(_ + 4))
    assertEquals(21, Iterator(1, 3, 5).sumBy(_ + 4))
    assertEquals(0, Vector[Int]().sumBy(_ + 4))
    assertEquals(0, Iterator[Int]().sumBy(_ + 4))
  }

  @Test
  def test_slidingN() {
    assertEqualsIterator(Iterator[(Int, Int)](), Iterator[Int]().sliding2)
    assertException(for (x <- Iterator(1).sliding2) {}) { case e: AssertionError => e.getMessage == "Cannot convert sequence of length 1 into Tuple2: List(1)" }
    assertEqualsIterator(Iterator((1, 2)), Iterator(1, 2).sliding2)
    assertEqualsIterator(Iterator((1, 2), (2, 3)), Iterator(1, 2, 3).sliding2)
    assertEqualsIterator(Iterator((1, 2), (2, 3), (3, 4)), Iterator(1, 2, 3, 4).sliding2)
    assertEqualsIterator(Iterator((1, 2), (2, 3), (3, 4), (4, 5)), Iterator(1, 2, 3, 4, 5).sliding2)

    assertEqualsIterator(Iterator[(Int, Int)](), Vector[Int]().sliding2)
    assertException(for (x <- Vector(1).sliding2) {}) { case e: AssertionError => e.getMessage == "Cannot convert sequence of length 1 into Tuple2: List(1)" }
    assertEqualsIterator(Iterator((1, 2)), Vector(1, 2).sliding2)
    assertEqualsIterator(Iterator((1, 2), (2, 3)), Vector(1, 2, 3).sliding2)
    assertEqualsIterator(Iterator((1, 2), (2, 3), (3, 4)), Vector(1, 2, 3, 4).sliding2)
    assertEqualsIterator(Iterator((1, 2), (2, 3), (3, 4), (4, 5)), Vector(1, 2, 3, 4, 5).sliding2)

    assertEqualsIterator(Iterator[(Int, Int, Int)](), Iterator[Int]().sliding3)
    assertException(for (x <- Iterator(1).sliding3) {}) { case e: AssertionError => e.getMessage == "Cannot convert sequence of length 1 into Tuple3: List(1)" }
    assertException(for (x <- Iterator(1, 2).sliding3) {}) { case e: AssertionError => e.getMessage == "Cannot convert sequence of length 2 into Tuple3: List(1,2)" }
    assertEqualsIterator(Iterator((1, 2, 3)), Iterator(1, 2, 3).sliding3)
    assertEqualsIterator(Iterator((1, 2, 3), (2, 3, 4)), Iterator(1, 2, 3, 4).sliding3)
    assertEqualsIterator(Iterator((1, 2, 3), (2, 3, 4), (3, 4, 5)), Iterator(1, 2, 3, 4, 5).sliding3)

    assertEqualsIterator(Iterator[(Int, Int, Int)](), Vector[Int]().sliding3)
    assertException(for (x <- Vector(1).sliding3) {}) { case e: AssertionError => e.getMessage == "Cannot convert sequence of length 1 into Tuple3: List(1)" }
    assertException(for (x <- Vector(1, 2).sliding3) {}) { case e: AssertionError => e.getMessage == "Cannot convert sequence of length 2 into Tuple3: List(1,2)" }
    assertEqualsIterator(Iterator((1, 2, 3)), Vector(1, 2, 3).sliding3)
    assertEqualsIterator(Iterator((1, 2, 3), (2, 3, 4)), Vector(1, 2, 3, 4).sliding3)
    assertEqualsIterator(Iterator((1, 2, 3), (2, 3, 4), (3, 4, 5)), Vector(1, 2, 3, 4, 5).sliding3)
  }

  @Test
  def test_slyce() {
    assertEquals(Vector(0, 1, 2, 3), (0 to 9).toVector.slyce(0, 4))
    assertEquals(Vector(6, 7, 8), (0 to 9).toVector.slyce(6, 9))
    assertEquals(Vector(3, 4, 5), (0 to 9).toVector.slyce(3, 6))
    assertEquals(Vector(), (0 to 9).toVector.slyce(6, 3))
    assertEquals(Vector(7, 8), (0 to 9).toVector.slyce(-3, -1))
    assertEquals(List(), (0 to 9).toList.slyce(-3, 2))
    assertEquals(List(5, 6), (0 to 9).toList.slyce(5, -3))
    assertEquals(List(4, 5, 6, 7), (0 to 9).toList.slyce(-6, 8))
    assertEquals(Vector(6, 7, 8, 9), (0 to 9).toVector.slyce(6, 12))

    assertEquals(Vector(0, 1, 2, 3), (0 to 9).toVector.slyce(0 until 4))
    assertEquals(Vector(6, 7, 8), (0 to 9).toVector.slyce(6 until 9))
    assertEquals(Vector(3, 4, 5), (0 to 9).toVector.slyce(3 until 6))
    assertEquals(Vector(), (0 to 9).toVector.slyce(6 until 3))
    assertEquals(Vector(7, 8), (0 to 9).toVector.slyce(-3 until -1))
    assertEquals(List(), (0 to 9).toList.slyce(-3 until 2))
    assertEquals(List(5, 6), (0 to 9).toList.slyce(5 until -3))
    assertEquals(List(4, 5, 6, 7), (0 to 9).toList.slyce(-6 until 8))
    assertEquals(Vector(6, 7, 8, 9), (0 to 9).toVector.slyce(6 until 12))

    assertEquals(Vector(0, 1, 2, 3, 4), (0 to 9).toVector.slyce(0 to 4))
    assertEquals(Vector(6, 7, 8, 9), (0 to 9).toVector.slyce(6 to 9))
    assertEquals(Vector(3, 4, 5, 6), (0 to 9).toVector.slyce(3 to 6))
    assertEquals(Vector(), (0 to 9).toVector.slyce(6 to 3))
    assertEquals(Vector(7, 8, 9), (0 to 9).toVector.slyce(-3 to -1))
    assertEquals(List(), (0 to 9).toList.slyce(-3 to 2))
    assertEquals(List(5, 6, 7), (0 to 9).toList.slyce(5 to -3))
    assertEquals(List(4, 5, 6, 7, 8), (0 to 9).toList.slyce(-6 to 8))
    assertEquals(Vector(6, 7, 8, 9), (0 to 9).toVector.slyce(6 to 12))

    //

    assertEqualsIterator(Iterator(0, 1, 2, 3), (0 to 9).iterator.slyce(0, 4))
    assertEqualsIterator(Iterator(6, 7, 8), (0 to 9).iterator.slyce(6, 9))
    assertEqualsIterator(Iterator(3, 4, 5), (0 to 9).iterator.slyce(3, 6))
    assertEqualsIterator(Iterator(), (0 to 9).iterator.slyce(6, 3))
    assertException((0 to 9).iterator.slyce(-3, -1)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertException((0 to 9).iterator.slyce(-3, 2)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertException((0 to 9).iterator.slyce(5, -3)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertException((0 to 9).iterator.slyce(-6, 8)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertEqualsIterator(Iterator(6, 7, 8, 9), (0 to 9).iterator.slyce(6, 12))

    assertEqualsIterator(Iterator(0, 1, 2, 3), (0 to 9).iterator.slyce(0 until 4))
    assertEqualsIterator(Iterator(6, 7, 8), (0 to 9).iterator.slyce(6 until 9))
    assertEqualsIterator(Iterator(3, 4, 5), (0 to 9).iterator.slyce(3 until 6))
    assertEqualsIterator(Iterator(), (0 to 9).iterator.slyce(6 until 3))
    assertException((0 to 9).iterator.slyce(-3 until -1)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertException((0 to 9).iterator.slyce(-3 until 2)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertException((0 to 9).iterator.slyce(5 until -3)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertException((0 to 9).iterator.slyce(-6 until 8)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertEqualsIterator(Iterator(6, 7, 8, 9), (0 to 9).iterator.slyce(6 until 12))

    assertEqualsIterator(Iterator(0, 1, 2, 3, 4), (0 to 9).iterator.slyce(0 to 4))
    assertEqualsIterator(Iterator(6, 7, 8, 9), (0 to 9).iterator.slyce(6 to 9))
    assertEqualsIterator(Iterator(3, 4, 5, 6), (0 to 9).iterator.slyce(3 to 6))
    assertEqualsIterator(Iterator(), (0 to 9).iterator.slyce(6 to 3))
    assertException((0 to 9).iterator.slyce(-3 to -1)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertException((0 to 9).iterator.slyce(-3 to 2)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertException((0 to 9).iterator.slyce(5 to -3)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertException((0 to 9).iterator.slyce(-6 to 8)) { case e: IllegalArgumentException => assertEquals("cannot slice Iterator with negative indices", e.getMessage) }
    assertEqualsIterator(Iterator(6, 7, 8, 9), (0 to 9).iterator.slyce(6 to 12))
  }

  @Test
  def test_countCompare() {
    var i = 0
    def it = Iterator(
      () => { i += 1; 1 },
      () => { i += 1; 2 },
      () => { i += 1; 3 },
      () => { i += 1; 4 },
      () => { i += 1; 5 },
      () => { i += 1; 6 },
      () => { i += 1; 7 },
      () => { i += 1; 9 }) //

    { val v = it; i = 0; assertTrue(v.countCompare(_.apply() % 2 == 0, 0) > 0); assertEquals(2, i); assertTrue(v.hasNext) } //
    { val v = it; i = 0; assertTrue(v.countCompare(_.apply() % 2 == 0, 1) > 0); assertEquals(4, i); assertTrue(v.hasNext) } //
    { val v = it; i = 0; assertTrue(v.countCompare(_.apply() % 2 == 0, 2) > 0); assertEquals(6, i); assertTrue(v.hasNext) } //
    { val v = it; i = 0; assertTrue(v.countCompare(_.apply() % 2 == 0, 3) == 0); assertEquals(8, i); assertFalse(v.hasNext) } //
    { val v = it; i = 0; assertTrue(v.countCompare(_.apply() % 2 == 0, 4) < 0); assertEquals(8, i); assertFalse(v.hasNext) } //
    { val v = it; i = 0; assertTrue(v.countCompare(_.apply() % 2 == 0, 5) < 0); assertEquals(8, i); assertFalse(v.hasNext) } //
  }

  @Test
  def test_takeWhileAg() {
    val v = Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9))

    assertEquals(Vector[Vector[Int]](), v.takeWhileAg(_.map(_.size).sum <= 0))
    assertEquals(Vector[Vector[Int]](), v.takeWhileAg(_.map(_.size).sum <= 1))
    assertEquals(Vector[Vector[Int]](), v.takeWhileAg(_.map(_.size).sum <= 2))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3)), v.takeWhileAg(_.map(_.size).sum <= 3))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3)), v.takeWhileAg(_.map(_.size).sum <= 4))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3)), v.takeWhileAg(_.map(_.size).sum <= 5))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.takeWhileAg(_.map(_.size).sum <= 6))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.takeWhileAg(_.map(_.size).sum <= 7))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.takeWhileAg(_.map(_.size).sum <= 8))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.takeWhileAg(_.map(_.size).sum <= 9))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.takeWhileAg(_.map(_.size).sum <= 10))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.takeWhileAg(_.map(_.size).sum <= Int.MaxValue))

    assertEqualsIterator(Iterator[Vector[Int]](), v.iterator.takeWhileAg(_.map(_.size).sum <= 0))
    assertEqualsIterator(Iterator[Vector[Int]](), v.iterator.takeWhileAg(_.map(_.size).sum <= 1))
    assertEqualsIterator(Iterator[Vector[Int]](), v.iterator.takeWhileAg(_.map(_.size).sum <= 2))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3)), v.iterator.takeWhileAg(_.map(_.size).sum <= 3))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3)), v.iterator.takeWhileAg(_.map(_.size).sum <= 4))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3)), v.iterator.takeWhileAg(_.map(_.size).sum <= 5))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.iterator.takeWhileAg(_.map(_.size).sum <= 6))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.iterator.takeWhileAg(_.map(_.size).sum <= 7))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.iterator.takeWhileAg(_.map(_.size).sum <= 8))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.iterator.takeWhileAg(_.map(_.size).sum <= 9))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.iterator.takeWhileAg(_.map(_.size).sum <= 10))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.iterator.takeWhileAg(_.map(_.size).sum <= Int.MaxValue))
  }

  @Test
  def test_takeSub() {
    val v = Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9))

    assertEquals(Vector[Vector[Int]](), v.takeSub(0))
    assertEquals(Vector[Vector[Int]](), v.takeSub(1))
    assertEquals(Vector[Vector[Int]](), v.takeSub(2))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3)), v.takeSub(3))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3)), v.takeSub(4))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3)), v.takeSub(5))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.takeSub(6))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.takeSub(7))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.takeSub(8))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.takeSub(9))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.takeSub(10))
    assertEquals(Vector[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.takeSub(Int.MaxValue))

    assertEqualsIterator(Iterator[Vector[Int]](), v.iterator.takeSub(0))
    assertEqualsIterator(Iterator[Vector[Int]](), v.iterator.takeSub(1))
    assertEqualsIterator(Iterator[Vector[Int]](), v.iterator.takeSub(2))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3)), v.iterator.takeSub(3))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3)), v.iterator.takeSub(4))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3)), v.iterator.takeSub(5))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.iterator.takeSub(6))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.iterator.takeSub(7))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6)), v.iterator.takeSub(8))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.iterator.takeSub(9))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.iterator.takeSub(10))
    assertEqualsIterator(Iterator[Vector[Int]](Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)), v.iterator.takeSub(Int.MaxValue))
  }

  @Test
  def test_ascDesc() {
    assertEquals(Vector('a -> 1, 'b -> 2, 'c -> 3), Map('b -> 2, 'a -> 1, 'c -> 3).asc)
    assertEquals(Vector('c -> 3, 'b -> 2, 'a -> 1), Map('b -> 2, 'a -> 1, 'c -> 3).desc)
  }

  @Test
  def test_Iterator_last() {
    { val i = Iterator("a", "b", "c"); assertEquals("c", i.last); assertFalse(i.hasNext) }
    { val i = Iterator("a"); assertEquals("a", i.last); assertFalse(i.hasNext) }
    { val i = Iterator[String](); assertException(i.last) { case e: AssertionError => assertEquals("cannot call Iterator.last on an empty iterator", e.getMessage) } }
  }

  @Test
  def test_Iterator_takeRight() {
    { val i = Iterator("a", "b", "c"); assertEquals(Vector("b", "c"), i.takeRight(2)); assertFalse(i.hasNext) }
    { val i = Iterator("a"); assertEquals(Vector("a"), i.takeRight(2)); assertFalse(i.hasNext) }
    { val i = Iterator[String](); assertEquals(Vector[String](), i.takeRight(2)); assertFalse(i.hasNext) }
    { val i = Iterator("a", "b", "c"); assertEquals(Vector[String](), i.takeRight(0)); assertFalse(i.hasNext) }
  }

  @Test
  def test_Iterator_dropRight() {
    { val i = Iterator("a", "b", "c"); assertEquals(Vector("a"), i.dropRight(2)); assertFalse(i.hasNext) }
    { val i = Iterator("a"); assertEquals(Vector[String](), i.dropRight(2)); assertFalse(i.hasNext) }
    { val i = Iterator[String](); assertEquals(Vector[String](), i.dropRight(2)); assertFalse(i.hasNext) }
    { val i = Iterator("a", "b", "c"); assertEquals(Vector("a", "b", "c"), i.dropRight(0)); assertFalse(i.hasNext) }
  }

  @Test
  def test_only() {
    { val i = Iterator("a"); assertEquals("a", i.only); assertFalse(i.hasNext) }
    { val i = Iterator("a", "b"); assertException(i.only) { case e: AssertionError => assertEquals("assertion failed: cannot call `only` on collection with 2 elements.", e.getMessage) }; assertFalse(i.hasNext) }
    { val i = Iterator("a", "b", "c"); assertException(i.only) { case e: AssertionError => assertEquals("assertion failed: cannot call `only` on collection with 3 elements.", e.getMessage) }; assertFalse(i.hasNext) }
    { val i = Iterator[String](); assertException(i.only) { case e: AssertionError => assertEquals("assertion failed: cannot call `only` on empty collection.", e.getMessage) } }

    { val v = Vector("a"); assertEquals("a", v.only) }
    { val v = Vector("a", "b"); assertException(v.only) { case e: AssertionError => assertEquals("assertion failed: cannot call `only` on collection with 2 elements.", e.getMessage) } }
    { val v = Vector("a", "b", "c"); assertException(v.only) { case e: AssertionError => assertEquals("assertion failed: cannot call `only` on collection with 3 elements.", e.getMessage) } }
    { val v = Vector[String](); assertException(v.only) { case e: AssertionError => assertEquals("assertion failed: cannot call `only` on empty collection.", e.getMessage) } }

    { val m = Vector("a" -> 1); assertEquals("a" -> 1, m.only) }
    { val m = Vector("a" -> 1, "b" -> 2); assertException(m.only) { case e: AssertionError => assertEquals("assertion failed: cannot call `only` on collection with 2 elements.", e.getMessage) } }
    { val m = Vector("a" -> 1, "b" -> 2, "c" -> 3); assertException(m.only) { case e: AssertionError => assertEquals("assertion failed: cannot call `only` on collection with 3 elements.", e.getMessage) } }
    { val m = Vector[(String, Int)](); assertException(m.only) { case e: AssertionError => assertEquals("assertion failed: cannot call `only` on empty collection.", e.getMessage) } }
  }

  @Test
  def test_mutableMapUpdateWith {
    val m = mutable.Map('a -> 1, 'b -> 2)
    val m1 = m.updateWith('a)(_ + 3)
    assertSame(m, m1)
    assertEquals(mutable.Map('a -> 4, 'b -> 2), m)
    val m2 = m.updateWith('b)(_ + 4)
    assertSame(m, m2)
    assertEquals(mutable.Map('a -> 4, 'b -> 6), m)
    assertExceptionMsg(f"key not found: 'c")(m.updateWith('c)(_ + 5))
  }

  @Test
  def test_mutableMapUpdateOrElseWith {
    val m = mutable.Map('a -> 1, 'b -> 2)
    val m1 = m.updateOrElseWith('a, 7)(_ + 3)
    assertSame(m, m1)
    assertEquals(mutable.Map('a -> 4, 'b -> 2), m)
    val m2 = m.updateOrElseWith('b, 8)(_ + 4)
    assertSame(m, m2)
    assertEquals(mutable.Map('a -> 4, 'b -> 6), m)
    val m3 = m.updateOrElseWith('c, 9)(_ + 5)
    assertSame(m, m3)
    assertEquals(mutable.Map('a -> 4, 'b -> 6, 'c -> 14), m)
  }
}
