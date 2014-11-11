package dhg.util

import org.junit.Assert._
import org.junit.Test
import dhg.util.CollectionUtil._
import dhg.util.CollectionUtil.KeepDelimiter._
import dhg.util.Collections._
import dhg.util.TestUtil._
import dhg.util.Pattern._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class PatternTests {

  @Test
  def test_UInt() {
    val UInt(a) = "15"
    assertEquals("int", a.getClass.getName)
    assertEquals(a, 15)

    val UInt(b) = "-15"
    assertEquals("int", b.getClass.getName)
    assertEquals(b, -15)

    val UDouble(c) = "-15"
    assertEquals("double", c.getClass.getName)
    assertEquals(c, -15.0, 0.0000001)

    val UDouble(d) = "-15.0"
    assertEquals("double", d.getClass.getName)
    assertEquals(d, -15.0, 0.0000001)

    val UDouble(e) = "-15."
    assertEquals("double", e.getClass.getName)
    assertEquals(e, -15.0, 0.0000001)

    val UDouble(f) = "-.15"
    assertEquals("double", f.getClass.getName)
    assertEquals(f, -0.15, 0.0000001)

    val UDouble(g) = "-15"
    assertEquals("double", g.getClass.getName)
    assertEquals(g, -15.0, 0.0000001)

    val UDouble(h) = "-15.0"
    assertEquals("double", h.getClass.getName)
    assertEquals(h, -15.0, 0.0000001)

    val UDouble(i) = "-15."
    assertEquals("double", i.getClass.getName)
    assertEquals(i, -15.0, 0.0000001)

    val UDouble(j) = "-.15"
    assertEquals("double", j.getClass.getName)
    assertEquals(j, -0.15, 0.0000001)
  }

  @Test
  def test_UBoolean() {
    val UBoolean(a) = "tRuE"
    assertEquals(true, a)
    val UBoolean(b) = "faLse"
    assertEquals(false, b)
  }

  @Test
  def test_Coll() {
    val Coll(g) = Set(7)
    assertEquals(7, g)

    val Coll((h, i)) = Map(7 -> 'g)
    assertEquals(7, h)
    assertEquals('g, i)

    val Coll(a, b, c @ _*) = Seq(1, 2, 3, 4, 5)
    assertEquals(1, a)
    assertEquals(2, b)
    assertEquals(Vector(3, 4, 5), c)

    val Coll(d, e, f @ _*) = Set(1, 2, 3, 4, 5)
    assertEquals(3, f.size)
    assertEquals(Set(1, 2, 3, 4, 5), Set(d, e) ++ f)
  }

  @Test
  def test_SetHeadTail {
    val s0 = Set.empty[Int]
    s0 match { case SetHeadTail(_, _) => fail(f"s=$s0"); case _ => }

    val s1 = Set(1)
    assertEquals(1, s1.size)
    val SetHeadTail(a1, bs1) = s1
    assertEquals(0, bs1.size)
    assertEquals(1, a1)
    assertEquals(Set.empty[Int], bs1)

    val s2 = Set(1, 2)
    assertEquals(2, s2.size)
    val SetHeadTail(a2, bs2) = s2
    assertEquals(1, bs2.size)
    assertEquals(s2, bs2 + a2)

    val s3 = (1 to 10).toSet
    assertEquals(10, s3.size)
    val SetHeadTail(a3, bs3) = s3
    assertEquals(9, bs3.size)
    assertEquals(s3, bs3 + a3)
  }

  @Test
  def test_arrow() {
    val a -> b = (1 -> 'a)
    assertEquals(1, a)
    assertEquals('a, b)
  }

  @Test
  def test_RangeString() {
    assertEquals(Vector(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13, 14), RangeString("1-3, 5, 6, 7-9,11-12, 13-14"))
    assertEquals("1-3,5-9,11-14", RangeString(Vector(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13, 14)))

    assertEquals(Vector(3), RangeString("3"))
    assertEquals(Vector(3, 4), RangeString("3-4"))
    assertEquals(Vector(3, 5), RangeString("3,5"))
    assertEquals(Vector(3, 4, 5), RangeString("3-5"))

    val RangeString(a) = Vector(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13, 14)
    assertEquals("1-3,5-9,11-14", a)
    val RangeString(b) = "1-3, 5, 6, 7-9,11-12, 13-14"
    assertEquals(Vector(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13, 14), b)
  }
}
