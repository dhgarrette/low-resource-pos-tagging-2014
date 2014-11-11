package dhg.util

import scala.collection.mutable.Buffer
import org.junit.Assert._
import org.junit.Test
import dhg.util.CollectionUtil._
import dhg.util.CollectionUtil.KeepDelimiter._
import dhg.util.Collections._
import dhg.util.TestUtil._
import dhg.util.StringUtil._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class StringUtilTests {

  @Test
  def test_rtrim() {
    assertEquals("", "".rtrim)
    assertEquals("this", "this".rtrim)
    assertEquals("this and  that", "this and  that".rtrim)
    assertEquals("this and  that", "this and  that ".rtrim)
    assertEquals("this and  that", "this and  that\t".rtrim)
    assertEquals("this and  that", "this and  that\n".rtrim)
    assertEquals("this and  that", "this and  that    \t \n  \t  ".rtrim)
  }

  @Test
  def test_splitlines() {
    assertEquals(Vector(""), "".splitlines)
    assertEquals(Vector(""), "\n".splitlines)
    assertEquals(Vector("this and", "that"), "this and\nthat".splitlines)
    assertEquals(Vector("this and ", " that"), "this and \n that".splitlines)
  }

  @Test
  def test_splitWhitespace() {
    assertEquals(Vector(""), "".splitWhitespace)
    assertEquals(Vector(""), " ".splitWhitespace)
    assertEquals(Vector("this", "and", "that"), "this and\nthat".splitWhitespace)
    assertEquals(Vector("this", "and", "that"), "this and \n that".splitWhitespace)
    assertEquals(Vector("this", "and", "that", "stuff"), "this   and \nthat\tstuff".splitWhitespace)
  }

  @Test
  def test_lsplit() {
    assertEquals("".split("x", 3).toVector, "".lsplit("x", 3))
    assertEquals("thisxthat".split("x", 3).toVector, "thisxthat".lsplit("x", 3))
    assertEquals("xxthisxxthatxxxstuffx".split("x").toVector, "xxthisxxthatxxxstuffx".lsplit("x"))
    assertEquals("xxthisxxthatxxxstuffx".split("x", 100).toVector, "xxthisxxthatxxxstuffx".lsplit("x", 100))
    assertEquals("xxthisxxthatxxxstuffxxxx".split("x").toVector, "xxthisxxthatxxxstuffxxxx".lsplit("x"))
    assertEquals("xxthisxxthatxxxstuffxxxx".split("x", 100).toVector, "xxthisxxthatxxxstuffxxxx".lsplit("x", 100))
    assertEquals("xxthisxxthatxxxstuffxxxx".split("x", 10).toVector, "xxthisxxthatxxxstuffxxxx".lsplit("x", 10))
    assertEquals("thisxandxxthatxandxstuff".split("x", 3).toVector, "thisxandxxthatxandxstuff".lsplit("x", 3))
    assertEquals("thisxandxxthatxandxxstuff".split("x", 3).toVector, "thisxandxxthatxandxxstuff".lsplit("x", 3))
    assertEquals("thisxandxxthatxandxxstuff".split("x+", 3).toVector, "thisxandxxthatxandxxstuff".lsplit("x+", 3))
    assertEquals("thisabcandabccthatabcandabcccstuff".split("abc+", 3).toVector, "thisabcandabccthatabcandabcccstuff".lsplit("abc+", 3))

    assertEquals(Vector("", "xxthis", "xxthat", "xxxstuff", "x"), "xxthisxxthatxxxstuffx".lsplit("x+", KeepDelimiterAsFirst))
    assertEquals(Vector("xx", "thisxx", "thatxxx", "stuffx"), "xxthisxxthatxxxstuffx".lsplit("x+", KeepDelimiterAsLast))
    assertEquals(Vector("", "xxthis", "xxthatxxxstuffx"), "xxthisxxthatxxxstuffx".lsplit("x+", 3, KeepDelimiterAsFirst))
    assertEquals(Vector("xx", "thisxx", "thatxxxstuffx"), "xxthisxxthatxxxstuffx".lsplit("x+", 3, KeepDelimiterAsLast))
  }

  @Test
  def test_rsplit() {
    assertEquals("".reverse.split("x", 3).map(_.reverse).reverse.toVector, "".rsplit("x", 3))
    assertEquals("thisxthat".reverse.split("x", 3).map(_.reverse).reverse.toVector, "thisxthat".rsplit("x", 3))
    assertEquals("xthisxxthatxxxstuffxx".reverse.split("x").map(_.reverse).reverse.toVector, "xthisxxthatxxxstuffxx".rsplit("x"))
    assertEquals("xthisxxthatxxxstuffxx".reverse.split("x", 100).map(_.reverse).reverse.toVector, "xthisxxthatxxxstuffxx".rsplit("x", 100))
    assertEquals("xxxxthisxxthatxxxstuffxx".reverse.split("x").map(_.reverse).reverse.toVector, "xxxxthisxxthatxxxstuffxx".rsplit("x"))
    assertEquals("xxxxthisxxthatxxxstuffxx".reverse.split("x", 100).map(_.reverse).reverse.toVector, "xxxxthisxxthatxxxstuffxx".rsplit("x", 100))
    assertEquals("xxxxthisxxthatxxxstuffxx".reverse.split("x", 10).map(_.reverse).reverse.toVector, "xxxxthisxxthatxxxstuffxx".rsplit("x", 10))
    assertEquals("thisxandxxthatxandxstuff".reverse.split("x", 3).map(_.reverse).reverse.toVector, "thisxandxxthatxandxstuff".rsplit("x", 3))
    assertEquals("thisxandxxthatxandxxstuff".reverse.split("x", 3).map(_.reverse).reverse.toVector, "thisxandxxthatxandxxstuff".rsplit("x", 3))
    assertEquals("thisxandxxthatxandxxstuff".reverse.split("x+", 3).map(_.reverse).reverse.toVector, "thisxandxxthatxandxxstuff".rsplit("x+", 3))
    assertEquals(Vector("thisabcandabccthat", "and", "stuff"), "thisabcandabccthatabcandabcccstuff".rsplit("abc+", 3))

    assertEquals(Vector("xxthis", "xxthat", "xxxstuff", "x"), "xxthisxxthatxxxstuffx".rsplit("x+", KeepDelimiterAsFirst))
    assertEquals(Vector("xx", "thisxx", "thatxxx", "stuffx", ""), "xxthisxxthatxxxstuffx".rsplit("x+", KeepDelimiterAsLast))
    assertEquals(Vector("xxthisxxthat", "xxxstuff", "x"), "xxthisxxthatxxxstuffx".rsplit("x+", 3, KeepDelimiterAsFirst))
    assertEquals(Vector("xxthisxxthatxxx", "stuffx", ""), "xxthisxxthatxxxstuffx".rsplit("x+", 3, KeepDelimiterAsLast))
  }

  @Test
  def test_padLeft() {
    assertEquals("  abc", "abc".padLeft(5))
    assertEquals("abc", "abc".padLeft(3))
    assertEquals("abc", "abc".padLeft(2))
    assertEquals("xyabc", "abc".padLeft(5, "xy"))
    assertEquals("yxyabc", "abc".padLeft(6, "xy"))
    assertEquals("xyxyabc", "abc".padLeft(7, "xy"))
  }

  @Test
  def test_padRight() {
    assertEquals("abc  ", "abc".padRight(5))
    assertEquals("abc", "abc".padRight(3))
    assertEquals("abc", "abc".padRight(2))
    assertEquals("abcxy", "abc".padRight(5, "xy"))
    assertEquals("abcxyx", "abc".padRight(6, "xy"))
    assertEquals("abcxyxy", "abc".padRight(7, "xy"))
  }

  @Test
  def test_wrapToLines() {
    assertEquals(Vector(""), "".wrapToLines(10))
    assertEquals(Vector("this is a", "test this", "is only a", "test"), "this is a test this is only a test".wrapToLines(10))
    assertEquals(Vector("this is a", "test", "this is", "only a", "test"), "this is a test\n this is only a test".wrapToLines(10))
  }

  @Test
  def test_wrap() {
    assertEquals("", "".wrap(10))
    assertEquals("this is a\ntest this\nis only a\ntest", "this is a test this is only a test".wrap(10))
    assertEquals("this is a\ntest\nthis is\nonly a\ntest", "this is a test\n this is only a test".wrap(10))
  }

  @Test
  def test_indent_spaces() {
    assertEquals("    ", "".indent(4))
    assertEquals("    this is a test this is only a test", "this is a test this is only a test".indent(4))
    assertEquals("    this is a test\n     this is only a\n    test", "this is a test\n this is only a\ntest".indent(4))
  }

  @Test
  def test_indent_string() {
    assertEquals(">>", "".indent(">>"))
    assertEquals(">>this is a test this is only a test", "this is a test this is only a test".indent(">>"))
    assertEquals(">>this is a test\n>> this is only a\n>>test", "this is a test\n this is only a\ntest".indent(">>"))
  }

  @Test
  def test_sideBySideStrings() {
    assertEquals("""
this is         a crazy
function that   aligns
                multiple columns
                of text""".split("\n").drop(1).mkString("\n"),
      sideBySideStrings(3, "this is\nfunction that", "a crazy\naligns\nmultiple columns\nof text"))
  }

  @Test
  def test_Regex_matches() {
    val r = "(.{2})(.{2})".r
    assertEquals(true, r.matches("abcd"))
    assertEquals(false, r.matches("abc"))
    assertEquals(false, r.matches("abcde"))
  }

  @Test
  def test_Regex_apply() {
    val r = "(.{2})(.{2})".r
    assertEquals(List("ab", "cd"), r("abcd"))
    assertException(r("abc")) { case e: IllegalStateException => assertEquals("No match found", e.getMessage) }
    assertException(r("abcde")) { case e: IllegalStateException => assertEquals("No match found", e.getMessage) }
  }

  @Test
  def test_Regex_groups() {
    val r = "(.{2})(.{2})".r
    assertEquals(List("ab", "cd"), r.groups("abcd"))
    assertException(r.groups("abc")) { case e: IllegalStateException => assertEquals("No match found", e.getMessage) }
    assertException(r.groups("abcde")) { case e: IllegalStateException => assertEquals("No match found", e.getMessage) }

    assertEquals(List("ab", "cd"), "abcd".groups(r))
    assertException("abc".groups(r)) { case e: IllegalStateException => assertEquals("No match found", e.getMessage) }
    assertException("abcde".groups(r)) { case e: IllegalStateException => assertEquals("No match found", e.getMessage) }
  }

  @Test
  def test_Regex_groupsOption() {
    val r = "(.{2})(.{2})".r
    assertEquals(Some(List("ab", "cd")), r.groupsOption("abcd"))
    assertEquals(None, r.groupsOption("abc"))
    assertEquals(None, r.groupsOption("abcde"))

    assertEquals(Some(List("ab", "cd")), "abcd".groupsOption(r))
    assertEquals(None, "abc".groupsOption(r))
    assertEquals(None, "abcde".groupsOption(r))
  }

  @Test
  def test_Regex_firstGroup() {
    val r = "(.{2})(.{2})".r
    assertEquals(Some(List("ab", "cd")), r.firstGroup("abcd"))
    assertEquals(None, r.firstGroup("abc"))
    assertEquals(Some(List("ab", "cd")), r.firstGroup("abcde"))
    assertEquals(Some(List("ab", "cd")), r.firstGroup("abcdefghi"))

    assertEquals(Some(List("ab", "cd")), "abcd".firstGroup(r))
    assertEquals(None, "abc".firstGroup(r))
    assertEquals(Some(List("ab", "cd")), "abcde".firstGroup(r))
    assertEquals(Some(List("ab", "cd")), "abcdefghi".firstGroup(r))
  }

  @Test
  def test_Regex_allGroups() {
    val r = "(.{2})(.{2})".r
    assertEqualsIterator(Iterator(List("ab", "cd")), r.allGroups("abcd"))
    assertEqualsIterator(Iterator(), r.allGroups("abc"))
    assertEqualsIterator(Iterator(List("ab", "cd")), r.allGroups("abcde"))
    assertEqualsIterator(Iterator(List("ab", "cd"), List("ef", "gh")), r.allGroups("abcdefghi"))

    assertEqualsIterator(Iterator(List("ab", "cd")), "abcd".allGroups(r))
    assertEqualsIterator(Iterator(), "abc".allGroups(r))
    assertEqualsIterator(Iterator(List("ab", "cd")), "abcde".allGroups(r))
    assertEqualsIterator(Iterator(List("ab", "cd"), List("ef", "gh")), "abcdefghi".allGroups(r))
  }

}
