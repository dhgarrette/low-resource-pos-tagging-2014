package dhg.util.math

import org.junit.Test
import dhg.util.TestUtil._
import dhg.util.StringUtil._
import dhg.util.CollectionUtil._
import math.{ log, exp, abs, pow }
import org.junit.Assert._
import Double.NaN
import scala.util.Random
import dhg.util.math.FastMathUtil._

class FastMathUtilTests {

  @Test
  def test_sum {
    assertException(sum(Array(arb, arb, arb), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in a length of 4 to sum, for an array of length 3", e.getMessage) }

    assertEquals(0.0, sum(Array[Double](), 0), 1e-9)
    assertEquals(0.0, sum(Array[Double](arb, arb), 0), 1e-9)
    assertEquals(2.0, sum(Array(2.0), 1), 1e-9)
    assertEquals(2.0, sum(Array(2.0, arb, arb), 1), 1e-9)
    assertEquals(3.0, sum(Array(1.0, 2.0), 2), 1e-9)
    assertEquals(3.0, sum(Array(1.0, 2.0, arb, arb), 2), 1e-9)
    assertEquals(0.0, sum(Array(0.0, 0.0, 0.0, arb, arb), 3), 1e-9)
    assertEquals(10.0, sum(Array(1.0, 5.0, 0.0, 2.0, 0.0, 2.0), 6), 1e-9)
    assertEquals(10.0, sum(Array(1.0, 5.0, 0.0, 2.0, 0.0, 2.0, arb, arb), 6), 1e-9)
    assertEquals(Double.PositiveInfinity, sum(Array(1.0, 5.0, Double.PositiveInfinity, 0.0, 2.0, 0.0, 2.0), 6), 1e-9)
    assertEquals(Double.PositiveInfinity, sum(Array(1.0, 5.0, Double.PositiveInfinity, 0.0, 2.0, 0.0, 2.0, arb, arb), 6), 1e-9)
  }

  @Test
  def test_activeSum {
    assertException(activeSum(Array(arb, arb, arb), Array(0, 1, 2, 3), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeSum, for an array of length 3", e.getMessage) }
    assertException(activeSum(Array(arb, arb, arb, arb), Array(0, 1, 2), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeSum, for an active array of length 3", e.getMessage) }

    assertEquals(0.0, activeSum(Array[Double](arb), Array[Int](0, 1), 0), 1e-9)
    assertEquals(2.0, activeSum(Array(arb, 2.0, arb), Array(1, 2, 3), 1), 1e-9)
    assertEquals(3.0, activeSum(Array(arb, 1.0, arb, 2.0, arb), Array(1, 3, 4, 5, 6), 2), 1e-9)
    assertEquals(0.0, activeSum(Array(arb, 0.0, arb, 0.0, 0.0, arb), Array(1, 3, 4, 5, 6, 7), 3), 1e-9)
    assertEquals(10.0, activeSum(Array(arb, 1.0, 5.0, 0.0, arb, 2.0, 0.0, 2.0, arb), Array(1, 2, 3, 5, 6, 7, 8, 9), 6), 1e-9)
    assertEquals(Double.PositiveInfinity, activeSum(Array(arb, 1.0, 5.0, Double.PositiveInfinity, 0.0, arb, 2.0, 0.0, 2.0, arb), Array(1, 2, 3, 4, 6, 7, 8, 9, 10, 11), 7), 1e-9)
  }

  @Test
  def test_logSum {
    assertEquals(3.0, exp(logSum(log(1.0), log(2.0))), 1e-9)
    assertEquals(3.0, exp(logSum(log(2.0), log(1.0))), 1e-9)
    assertEquals(1.0, exp(logSum(log(1.0), log(0.0))), 1e-9)
    assertEquals(2.0, exp(logSum(log(0.0), log(2.0))), 1e-9)

    assertEquals(10.0, exp(logSum(log(1.0), log(5.0), log(0.0), log(2.0), log(0.0), log(2.0))), 1e-9)
    assertEquals(Double.PositiveInfinity, exp(logSum(log(1.0), log(5.0), Double.PositiveInfinity, log(0.0), log(2.0), log(0.0), log(2.0))), 1e-9)

    assertEquals(10.0, /*              */ exp(logSum(Iterator(log(1.0), log(5.0), log(0.0), log(2.0), log(0.0), log(2.0)), log(5.0))), 1e-9)
    assertEquals(Double.PositiveInfinity, exp(logSum(Iterator(log(1.0), log(5.0), Double.PositiveInfinity, log(0.0), log(2.0), log(0.0), log(2.0)), log(Double.PositiveInfinity))), 1e-9)

    assertEquals(0.0, exp(logSum(Seq())), 1e-9)
    assertEquals(2.0, exp(logSum(Seq(log(2.0)))), 1e-9)
    assertEquals(3.0, exp(logSum(Seq(log(1.0), log(2.0)))), 1e-9)
    assertEquals(10.0, exp(logSum(Seq(log(1.0), log(5.0), log(0.0), log(2.0), log(0.0), log(2.0)))), 1e-9)
    assertEquals(Double.PositiveInfinity, exp(logSum(Seq(log(1.0), log(5.0), Double.PositiveInfinity, log(0.0), log(2.0), log(0.0), log(2.0)))), 1e-9)

    assertException(logSum(Array(arb, arb, arb), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in a length of 4 to logSum, for an array of length 3", e.getMessage) }
    assertEquals(0.0, exp(logSum(Array[Double](arb, arb), 0)), 1e-9)
    assertEquals(2.0, exp(logSum(Array(log(2.0), arb, arb), 1)), 1e-9)
    assertEquals(3.0, exp(logSum(Array(log(1.0), log(2.0), arb, arb), 2)), 1e-9)
    assertEquals(0.0, exp(logSum(Array(log(0.0), log(0.0), log(0.0), arb, arb), 3)), 1e-9)
    assertEquals(10.0, exp(logSum(Array(log(1.0), log(5.0), log(0.0), log(2.0), log(0.0), log(2.0), arb, arb), 6)), 1e-9)
    assertEquals(Double.PositiveInfinity, exp(logSum(Array(log(1.0), log(5.0), Double.PositiveInfinity, log(0.0), log(2.0), log(0.0), log(2.0), arb, arb), 6)), 1e-9)
  }

  @Test
  def test_activeLogSum {
    assertException(activeLogSum(Array(arb, arb, arb), Array(0, 1, 2, 3), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeLogSum, for an array of length 3", e.getMessage) }
    assertException(activeLogSum(Array(arb, arb, arb, arb), Array(0, 1, 2), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeLogSum, for an active array of length 3", e.getMessage) }

    assertEquals(0.0, exp(activeLogSum(Array[Double](arb).map(log), Array[Int](0, 1), 0)), 1e-9)
    assertEquals(2.0, exp(activeLogSum(Array(arb, 2.0, arb).map(log), Array(1, 2, 3), 1)), 1e-9)
    assertEquals(3.0, exp(activeLogSum(Array(arb, 1.0, arb, 2.0, arb).map(log), Array(1, 3, 4, 5), 2)), 1e-9)
    assertEquals(0.0, exp(activeLogSum(Array(arb, 0.0, arb, 0.0, 0.0, arb).map(log), Array(1, 3, 4, 5, 6, 7), 3)), 1e-9)
    assertEquals(10.0, exp(activeLogSum(Array(arb, 1.0, 5.0, 0.0, arb, 2.0, 0.0, 2.0, arb).map(log), Array(1, 2, 3, 5, 6, 7, 8, 9), 6)), 1e-9)
    assertEquals(Double.PositiveInfinity, exp(activeLogSum(Array(arb, 1.0, 5.0, Double.PositiveInfinity, 0.0, arb, 2.0, 0.0, 2.0, arb).map(log), Array(1, 2, 3, 4, 6, 7, 8, 9, 10), 7)), 1e-9)

    assertEquals(log(0), activeLogSum(Array[Double](), Array(1, 2), 0), 1e-9)
    assertEquals(log(0), activeLogSum(Array[Double](arb, arb, arb), Array(1, 2), 0), 1e-9)
    assertEquals(log(3), activeLogSum(Array[Double](3).map(log), Array(0, 1, 2), 1), 1e-9)
    assertEquals(log(7), activeLogSum(Array[Double](3, 4).map(log), Array(0, 1, 2, 3), 2), 1e-9)
    assertEquals(log(15), activeLogSum(Array[Double](3, 4, 7, 1).map(log), Array(0, 1, 2, 3, 4, 5), 4), 1e-9)
    assertEquals(log(15), activeLogSum(Array[Double](3, 4, 7, 1, 6, 8, 5, 2).map(log), Array(0, 1, 2, 3, 4, 5), 4), 1e-9)
    assertEquals(log(4 + 1 + 6 + 5), activeLogSum(Array[Double](6, 4, 7, 1, 6, 8, 5, 2).map(log), Array(1, 3, 4, 6, 7, 8), 4), 1e-9)
    assertEquals(log(4 + 1 + 6 + 5), activeLogSum(Array[Double](6, 4, 7, 1, 6, 8, 5, 2).map(log), Array(1, 3, 4, 6, 7, 8, 9, 10), 4), 1e-9)
  }

  @Test
  def test_max {
    assertException(max(Array[Double](), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot compute max for a length of zero. (Array has length 0)", e.getMessage) }
    assertException(max(Array(1.0, 2.0), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot compute max for a length of zero. (Array has length 2)", e.getMessage) }
    assertException(max(Array(1.0, 2.0), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in a length of 4 to max, for an array of length 2", e.getMessage) }

    assertEquals(2.0, max(Array(2.0, arb, arb), 1), 1e-9)
    assertEquals(2.0, max(Array(1.0, 2.0, arb, arb), 2), 1e-9)
    assertEquals(5.0, max(Array(1.0, 5.0, 0.0, 2.0, 0.0, 2.0, arb, arb), 6), 1e-9)
    assertEquals(Double.PositiveInfinity, max(Array(1.0, 5.0, Double.PositiveInfinity, 0.0, 2.0, 0.0, 2.0, arb, arb), 6), 1e-9)
  }

  @Test
  def test_activeMax {
    assertException(activeMax(Array[Double](arb, arb), Array(0, 1, 2, 3), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot compute activeMax for an activeCount of zero. (Active array has length 4)", e.getMessage) }
    assertException(activeMax(Array(arb, arb, arb, arb), Array(0, 1), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeMax, for an active array of length 2", e.getMessage) }
    assertException(activeMax(Array(arb, arb), Array(0, 1, 2, 3), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeMax, for an array of length 2", e.getMessage) }

    assertEquals(2.0, activeMax(Array(arb, 2.0, arb), Array(1, 2, 3, 4), 1), 1e-9)
    assertEquals(2.0, activeMax(Array(arb, 1.0, arb, 2.0, arb), Array(1, 3, 4, 5, 6, 7, 8), 2), 1e-9)
    assertEquals(5.0, activeMax(Array(arb, 1.0, 5.0, 0.0, arb, 2.0, 0.0, 2.0, arb), Array(1, 2, 3, 5, 6, 7, 8, 9), 6), 1e-9)
    assertEquals(Double.PositiveInfinity, activeMax(Array(arb, 1.0, 5.0, Double.PositiveInfinity, 0.0, arb, 2.0, 0.0, 2.0, arb), Array(1, 2, 3, 4, 6, 7, 8, 9, 10, 11), 6), 1e-9)

    assertEquals(3, activeMax(Array(3), Array(0), 1), 1e-9)
    assertEquals(3, activeMax(Array(3), Array(0, 5, 6, 7), 1), 1e-9)
    assertEquals(5, activeMax(Array(3, 5, 7), Array(1, 5, 6, 7), 1), 1e-9)
    assertEquals(4, activeMax(Array(3, 4), Array(0, 1), 2), 1e-9)
    assertEquals(7, activeMax(Array(3, 4, 7, 1), Array(0, 1, 2, 3), 4), 1e-9)
    assertEquals(7, activeMax(Array(3, 4, 7, 1, 6, 8, 5, 2), Array(0, 1, 2, 3), 4), 1e-9)
    assertEquals(6, activeMax(Array(6, 4, 7, 1, 6, 8, 5, 2), Array(1, 3, 4, 6), 4), 1e-9)
    assertEquals(6, activeMax(Array(6, 4, 7, 1, 6, 8, 5, 2), Array(1, 3, 4, 6, 7, 8), 4), 1e-9)
  }

  @Test
  def test_argmax {
    assertException(argmax(Array[Double](), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot compute argmax for a length of zero. (Array has length 0)", e.getMessage) }
    assertException(argmax(Array(1.0, 2.0), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot compute argmax for a length of zero. (Array has length 2)", e.getMessage) }
    assertException(argmax(Array(1.0, 2.0), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in a length of 4 to argmax, for an array of length 2", e.getMessage) }

    assertEquals(0, argmax(Array(3), 1), 1e-9)
    assertEquals(0, argmax(Array(3, 5, 7), 1), 1e-9)
    assertEquals(1, argmax(Array(3, 4), 2), 1e-9)
    assertEquals(2, argmax(Array(3, 4, 7, 1), 4), 1e-9)
    assertEquals(2, argmax(Array(3, 4, 7, 1, 6, 8, 5, 2), 4), 1e-9)
    assertEquals(5, argmax(Array(3, 4, 7, 1, 6, 8, 5, 2), 8), 1e-9)
    assertEquals(2, argmax(Array(6, 4, 8, 1, 6, 8, 5, 2), 8), 1e-9)
  }

  @Test
  def test_activeArgmax {
    assertException(activeArgmax(Array[Double](arb, arb), Array(0, 1, 2, 3), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot compute activeArgmax for an activeCount of zero. (Active array has length 4)", e.getMessage) }
    assertException(activeArgmax(Array(arb, arb, arb, arb), Array(0, 1), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeArgmax, for an active array of length 2", e.getMessage) }
    assertException(activeArgmax(Array(arb, arb), Array(0, 1, 2, 3), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeArgmax, for an array of length 2", e.getMessage) }

    assertEquals(0, activeArgmax(Array(3), Array(0), 1), 1e-9)
    assertEquals(0, activeArgmax(Array(3), Array(0, 5, 6, 7), 1), 1e-9)
    assertEquals(1, activeArgmax(Array(3, 5, 7), Array(1, 5, 6, 7), 1), 1e-9)
    assertEquals(1, activeArgmax(Array(3, 4), Array(0, 1), 2), 1e-9)
    assertEquals(2, activeArgmax(Array(3, 4, 7, 1), Array(0, 1, 2, 3), 4), 1e-9)
    assertEquals(2, activeArgmax(Array(3, 4, 7, 1, 6, 8, 5, 2), Array(0, 1, 2, 3), 4), 1e-9)
    assertEquals(4, activeArgmax(Array(6, 4, 7, 1, 6, 8, 5, 2), Array(1, 3, 4, 6), 4), 1e-9)
    assertEquals(4, activeArgmax(Array(6, 4, 7, 1, 6, 8, 5, 2), Array(1, 3, 4, 6, 7, 8), 4), 1e-9)
    assertEquals(2, activeArgmax(Array(6, 4, 6, 1, 6, 8, 5, 2), Array(1, 2, 3, 4, 6), 5), 1e-9)
  }

  @Test
  def test_normalize {
    assertException(normalize(Array(arb, arb), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot normalize for a length of zero. (Array has length 2)", e.getMessage) }
    assertException(normalize(Array(arb, arb, arb), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in a length of 4 to sum, for an array of length 3", e.getMessage) }

    val a1 = Array(4, arb, arb, arb)
    normalize(a1, 1)
    assertEquals(1.0, a1(0), 1e-9)

    val a2 = Array(4, 10, 6, arb, arb)
    normalize(a2, 3)
    assertEquals(0.2, a2(0), 1e-9)
    assertEquals(0.5, a2(1), 1e-9)
    assertEquals(0.3, a2(2), 1e-9)
  }

  @Test
  def test_activeNormalize {
    assertException(activeNormalize(Array(arb, arb), Array(0, 1, 2), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot activeNormalize for an activeCount of zero. (Active array has length 3)", e.getMessage) }
    assertException(activeNormalize(Array(arb, arb, arb), Array(0, 1, 2, 3), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeSum, for an array of length 3", e.getMessage) }
    assertException(activeNormalize(Array(arb, arb, arb, arb), Array(0, 1, 2), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeSum, for an active array of length 3", e.getMessage) }

    val a1 = Array(arb, 4, arb, arb, arb)
    activeNormalize(a1, Array(1, 2, 3, 4), 1)
    assertEquals(1.0, a1(1), 1e-9)

    val a2 = Array(arb, 4, 10, arb, 6, arb, arb)
    activeNormalize(a2, Array(1, 2, 4, 5, 6, 7, 8), 3)
    assertEquals(0.2, a2(1), 1e-9)
    assertEquals(0.5, a2(2), 1e-9)
    assertEquals(0.3, a2(4), 1e-9)
  }

  @Test
  def test_normalizeAndLog {
    assertException(normalizeAndLog(Array(arb, arb), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot normalizeAndLog for a length of zero. (Array has length 2)", e.getMessage) }
    assertException(normalizeAndLog(Array(arb, arb, arb), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in a length of 4 to sum, for an array of length 3", e.getMessage) }

    val a1 = Array(4, arb, arb, arb)
    normalizeAndLog(a1, 1)
    assertEquals(log(1.0), a1(0), 1e-9)

    val a2 = Array(4, 10, 6, arb, arb)
    normalizeAndLog(a2, 3)
    assertEquals(log(0.2), a2(0), 1e-9)
    assertEquals(log(0.5), a2(1), 1e-9)
    assertEquals(log(0.3), a2(2), 1e-9)
  }

  @Test
  def test_activeNormalizeAndLog {
    assertException(activeNormalizeAndLog(Array(arb, arb), Array(0, 1, 2), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot activeNormalizeAndLog for an activeCount of zero. (Active array has length 3)", e.getMessage) }
    assertException(activeNormalizeAndLog(Array(arb, arb, arb), Array(0, 1, 2, 3), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeSum, for an array of length 3", e.getMessage) }
    assertException(activeNormalizeAndLog(Array(arb, arb, arb, arb), Array(0, 1, 2), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeSum, for an active array of length 3", e.getMessage) }

    val a1 = Array(arb, 4, arb, arb, arb)
    activeNormalizeAndLog(a1, Array(1, 2, 3, 4), 1)
    assertEquals(log(1.0), a1(1), 1e-9)

    val a2 = Array(arb, 4, 10, arb, 6, arb, arb)
    activeNormalizeAndLog(a2, Array(1, 2, 4, 5, 6, 7, 8), 3)
    assertEquals(log(0.2), a2(1), 1e-9)
    assertEquals(log(0.5), a2(2), 1e-9)
    assertEquals(log(0.3), a2(4), 1e-9)
  }

  @Test
  def test_logNormalize {
    assertException(logNormalize(Array(arb, arb), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot logNormalize for a length of zero. (Array has length 2)", e.getMessage) }
    assertException(logNormalize(Array(arb, arb, arb), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in a length of 4 to logSum, for an array of length 3", e.getMessage) }

    val a1 = Array(4, arb, arb, arb).map(log)
    logNormalize(a1, 1)
    assertEquals(log(1.0), a1(0), 1e-9)

    val a2 = Array(4, 10, 6, arb, arb).map(log)
    logNormalize(a2, 3)
    assertEquals(log(0.2), a2(0), 1e-9)
    assertEquals(log(0.5), a2(1), 1e-9)
    assertEquals(log(0.3), a2(2), 1e-9)
  }

  @Test
  def test_activeLogNormalize {
    assertException(activeLogNormalize(Array(arb, arb), Array(0, 1, 2), 0)) { case e: AssertionError => assertEquals("assertion failed: Cannot activeLogNormalize for an activeCount of zero. (Active array has length 3)", e.getMessage) }
    assertException(activeLogNormalize(Array(arb, arb, arb), Array(0, 1, 2, 3), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeLogSum, for an array of length 3", e.getMessage) }
    assertException(activeLogNormalize(Array(arb, arb, arb, arb), Array(0, 1, 2), 4)) { case e: AssertionError => assertEquals("assertion failed: Passed in an activeCount of 4 to activeLogSum, for an active array of length 3", e.getMessage) }

    val a1 = Array(arb, 4, arb, arb, arb).map(log)
    activeLogNormalize(a1, Array(1, 2, 3, 4), 1)
    assertEquals(log(1.0), a1(1), 1e-9)

    val a2 = Array(arb, 4, 10, arb, 6, arb, arb).map(log)
    activeLogNormalize(a2, Array(1, 2, 4, 5, 6, 7, 8), 3)
    assertEquals(log(0.2), a2(1), 1e-9)
    assertEquals(log(0.5), a2(2), 1e-9)
    assertEquals(log(0.3), a2(4), 1e-9)
  }

  def arb = Random.nextDouble * 20

}
