package dhg.util

import org.junit.Assert._
import dhg.util.CollectionUtil._
import dhg.util.StringUtil._
import dhg.util.math.LogDouble
import dhg.util.math.LogDouble._
import scala.math.log
import java.lang.AssertionError
import scala.util.matching.Regex

/**
 * Test utilities
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object TestUtil {

  private[this] class TestUtilExceptionNotFound extends RuntimeException("exception expected, but no exception thrown")

  def assertEqualsDouble(expected: Double, actual: Double) {
    assertEquals(expected, actual, 0.000000001)
  }

  def assertException(block: => Unit)(handle: PartialFunction[Throwable, Unit]) {
    try { block; throw new TestUtilExceptionNotFound } catch {
      case e: TestUtilExceptionNotFound => fail(e.getMessage)
      case e: Throwable => handle(e)
    }
  }

  def assertExceptionAny(block: => Unit) {
    try { block; throw new TestUtilExceptionNotFound } catch {
      case e: TestUtilExceptionNotFound => fail(e.getMessage)
      case e: Throwable =>
    }
  }

  def assertExceptionMsg(expectedMessage: String)(block: => Unit) {
    try { block; throw new TestUtilExceptionNotFound } catch {
      case e: TestUtilExceptionNotFound => fail(e.getMessage)
      case e: Throwable => assertEquals(expectedMessage, e.getMessage)
    }
  }

  def assertExceptionMsg(expectedMessageRe: Regex)(block: => Unit) {
    try { block; throw new TestUtilExceptionNotFound } catch {
      case e: TestUtilExceptionNotFound => fail(e.getMessage)
      case e: Throwable => assertMatch(expectedMessageRe, e.getMessage)
    }
  }

  def assertMatch(expectedRe: Regex, result: String) = {
    if (!expectedRe.matches(result))
      throw new AssertionError(s"expected match: $expectedRe but was: $result")
  }

  def assertEqualsArray[A](expected: Array[A], result: Array[A]) {
    if (expected.size != result.size || (expected zip result).exists(p => p._1 != p._2))
      throw new AssertionError(s"expected: Array(${expected.mkString(",")}) but was: Array(${result.mkString(",")})")
  }

  def assertEqualsIterator[A](expected: Iterator[A], result: Iterator[A]) {
    var i = 0
    while (expected.hasNext && result.hasNext) {
      assertEquals("mismatch on element " + i, expected.next, result.next)
      i += 1
    }
    if (expected.hasNext)
      fail("expected still contains: [%s]".format(expected.toSeq.mkString(", ")))
    if (result.hasNext)
      fail("result still contains: [%s]".format(result.toSeq.mkString(", ")))
  }

  def assertEqualsSameElements[T: Ordering](expected: Seq[T], actual: Seq[T]) {
    assertEquals("%s vs %s: DIFFERENCE: (%s)".format(expected, actual, (expected ++ actual).toSet -- (expected.toSet & actual.toSet)), expected.sorted.size, actual.sorted.size)
    for ((e, a) <- expected.sorted zipSafe actual.sorted)
      assertEquals(e, a)
  }

  def assertEqualsSmart[T](expected: (T, Double), actual: (T, Double)) {
    assertEquals(expected._1, actual._1)
    assertEqualsDouble(expected._2, actual._2)
  }

  def assertEqualsSmartLog[T](expected: (T, Double), actual: (T, LogDouble)) {
    assertEquals(expected._1, actual._1)
    assertEqualsDouble(log(expected._2), actual._2.logValue)
  }

  def assertEqualsSmart[T](expected: Double, actual: LogDouble) {
    assertEqualsDouble(log(expected), actual.logValue)
  }

  def assertEqualsSmart[T](expected: Map[T, Double], actual: Map[T, Double]) {
    def keystr(m: Map[T, Double]) = s"${m.keys.toVector.map(_.toString).sorted.mkString(", ")}"
    assertEquals("Wrong keys.", keystr(expected), keystr(actual))
    for ((k, ev) <- expected)
      assertEqualsDouble(ev, actual(k))
  }

  def assertEqualsSmartLog[T](expected: Map[T, Double], actual: Map[T, LogDouble]) {
    def keystr(m: Map[T, _]) = s"${m.keys.toVector.map(_.toString).sorted.mkString(", ")}"
    assertEquals("Wrong keys.", keystr(expected), keystr(actual))
    for ((k, ev) <- expected)
      assertEqualsDouble(log(ev), actual(k).logValue)
  }

  def assertEqualsSmart[A](expected: Option[Map[A, Double]], actual: Option[Map[A, Double]]) {
    assertEquals(expected.isDefined, actual.isDefined)
    if (expected.isDefined) assertEqualsSmart(expected.get, actual.get)
  }

  def assertEqualsSmart[A, B](expected: Vector[(A, Map[B, Double])], actual: Vector[(A, Map[B, Double])]) {
    assertEquals(expected.size, actual.size)
    for (((eA, eB), (aA, aB)) <- expected zip actual) {
      assertEquals(eA, aA)
      assertEqualsSmart(eB, aB)
    }
  }

  //  def dumpToFile(lines: TraversableOnce[String]): String = {
  //    val tempFile = mktemp("tmp")
  //    writeUsing(tempFile) { f => lines.foreach(l => f.write(l + "\n")) }
  //    tempFile.path
  //  }

  //  def dumpToFile(lines: String): String =
  //    dumpToFile(lines.split("\n").map(_.trim).filter(_.nonEmpty))

  //  def printFile(filename: String) =
  //    File(filename).readLines.foreach(println)

  //  def readFile(filename: String) =
  //    File(filename).readLines.mkString("\n")

  //  def assertFile(expected: String, filename: String) = {
  //    val e = expected.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n")
  //    val f = readFile(filename).split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n")
  //    //e.zip(f).zipWithIndex.foreach { case ((a, b), i) => assertEquals(a, b) } //"char at [%d]: [%s] vs [%s]".format(i,a,b)
  //    assertEquals(e, f)
  //  }

  //  def assertPath(expected: String, filename: String) =
  //    assertEquals(File(expected).getAbsolutePath, File(filename).getAbsolutePath)

}
