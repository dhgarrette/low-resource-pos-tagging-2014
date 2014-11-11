package dhg.util.math

import scala.math._
import org.junit.Assert._
import org.junit.Test
import dhg.util.math.LogDouble._
import scalaz._
import Scalaz._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class LogDoubleTests {

  @Test
  def test_sum() {
    val a = new LogDouble(-772.740105887932)
    val l = List(a)
    val s = LogDouble.zero + a
    assertTrue(s != LogDouble.zero)
  }

  @Test
  def test_operators() {
    val a: LogDouble = LogDouble(3)
    val b: LogDouble = LogDouble(5)
    val c: LogDouble = LogDouble(6)
    val d: LogDouble = LogDouble(2)
    val e: LogDouble = LogDouble(8)

    assertEquals(log(3.0), (a.logValue: Double), 0.0000001)
    assertEquals(3.0, (a.toDouble: Double), 0.0000001)
    assertEquals(3, (a.toInt: Int), 0.0000001)

    assertEqualsLog(b, d + a)
    assertEqualsLog(d, b - a)
    assertEqualsLog(c, d * a)
    assertEqualsLog(d, c / a)
    assertEqualsLog(e, d ** 3)
    assertEqualsLog(e, d ** 3.0)

    assertEqualsLog(b, a max b)
    assertEqualsLog(b, b max a)
    assertEqualsLog(a, a min b)
    assertEqualsLog(a, b min a)

    assertTrue(a < b)
    assertFalse(a > b)
    assertTrue(b > a)
    assertFalse(b < a)

    assertEqualsLog(LogDouble.apply(10), List(a, b, d).sum)
    assertEqualsNumericLog(b, List(a, b).max)
    assertEqualsNumericLog(b, List(b, a).max)
    assertEqualsNumericLog(a, List(a, b).min)
    assertEqualsNumericLog(a, List(b, a).min)
  }

  @Test
  def test_LogDouble_static_sum() {
    assertEqualsLog(LogDouble(15.0), LogDouble.sum(Vector(3.5, 7.5, 4.0).map(LogDouble(_))))
    assertEqualsLog(LogDouble(11.0), LogDouble.sum(Vector(3.5, 7.5).map(LogDouble(_))))
    assertEqualsLog(LogDouble(3.5), LogDouble.sum(Vector(3.5).map(LogDouble(_))))
    assertEqualsLog(LogDouble.zero, LogDouble.sum(Vector.empty[LogDouble]))
  }

  @Test
  def test_numeric() {
    val a: LogDouble = LogDouble(3)
    val b: LogDouble = LogDouble(5)
    val c: LogDouble = LogDouble(6)
    val d: LogDouble = LogDouble(2)
    val e: LogDouble = LogDouble(8)

    def stuff[N](a: N, b: N, c: N, d: N, e: N)(implicit num: Fractional[N]) = {
      assertEqualsNumericLog(b, num.plus(d, a))
      assertEqualsNumericLog(d, num.minus(b, a))
      assertEqualsNumericLog(c, num.times(d, a))
      assertEqualsNumericLog(d, num.div(c, a))

      assertEqualsNumericLog(b, List(a, b).max)
      assertEqualsNumericLog(b, List(b, a).max)
      assertEqualsNumericLog(a, List(a, b).min)
      assertEqualsNumericLog(a, List(b, a).min)
    }

    stuff(a, b, c, d, e)
  }

  @Test
  def test_semigroup_append() {
    val a: LogDouble = LogDouble(3)
    val b: LogDouble = LogDouble(5)
    val c: LogDouble = LogDouble(8)
    assertEqualsLog(c, a |+| b)
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

  def assertEqualsNumericLog[N](a: N, b: N)(implicit num: Numeric[N]) {
    (a, b) match {
      case (a: LogDouble, b: LogDouble) => assertEqualsLog(a, b)
      case _ => fail(s"a and b are not LogDouble instances: a:[$a], b:[$b]")
    }
  }

}
