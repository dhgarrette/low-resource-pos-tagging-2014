package dhg.util

import dhg.util.CollectionUtil._
import scala.util.matching.Regex

/**
 * Enhancement methods for Strings
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object StringUtil {

  val WhitespaceRe = """\s*""".r
  val RTrimRe = """(.*\S)\s*""".r

  implicit class EnrichedString(val self: String) extends AnyVal {

    /**
     * Trim whitespace only from the right side of the string
     */
    def rtrim = self match {
      case WhitespaceRe() => ""
      case RTrimRe(trimmed) => trimmed
    }

    /**
     * Split on newlines
     */
    def splitlines: Vector[String] = self.lsplit("\n")

    /**
     * Split on whitespace
     */
    def splitWhitespace: Vector[String] = self.lsplit("\\s+")

    /**
     * Split a string into `limit` pieces, starting from the left side.
     */
    def lsplit(str: String, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter): Vector[String] = {
      new RegexMatcherSplitIterator(self, str, keepDelimiter).toVector
        .dropRightWhile { case (b, e) => b == e && b > 0 }
        .map { case (b, e) => self.substring(b, e) }
    }

    /**
     * Split a string into `limit` pieces, starting from the left side.
     */
    def lsplit(str: String, limit: Int): Vector[String] = {
      lsplit(str, limit, KeepDelimiter.DropDelimiter)
    }

    /**
     * Split a string into `limit` pieces, starting from the left side.
     */
    def lsplit(str: String, limit: Int, keepDelimiter: KeepDelimiter): Vector[String] = {
      val allSpans = new RegexMatcherSplitIterator(self, str, keepDelimiter).take(limit).toVector
      val leftSpans :+ h = allSpans
      val spans = leftSpans :+ (h._1 -> self.length())
      spans.map { case (b, e) => self.substring(b, e) }
    }

    /**
     * Split a string into `limit` pieces, starting from the right side.
     */
    def rsplit(str: String, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter): Vector[String] = {
      new RegexMatcherSplitIterator(self, str, keepDelimiter).toVector
        .dropWhile { case (b, e) => b == e }
        .map { case (b, e) => self.substring(b, e) }
    }

    /**
     * Split a string into `limit` pieces, starting from the right side.
     */
    def rsplit(str: String, limit: Int): Vector[String] = {
      rsplit(str, limit, KeepDelimiter.DropDelimiter)
    }

    /**
     * Split a string into `limit` pieces, starting from the right side.
     */
    def rsplit(str: String, limit: Int, keepDelimiter: KeepDelimiter): Vector[String] = {
      val allSpans = new RegexMatcherSplitIterator(self, str, keepDelimiter).toVector
      val spans =
        if (allSpans.size > limit) {
          val h +: rightSpans = allSpans.takeRight(limit)
          (0 -> h._2) +: rightSpans
        }
        else
          allSpans
      spans.map { case (b, e) => self.substring(b, e) }
    }

    def groups(regex: Regex) = regex.groups(self)
    def groupsOption(regex: Regex) = regex.groupsOption(self)
    def firstGroup(regex: Regex) = regex.firstGroup(self)
    def allGroups(regex: Regex) = regex.allGroups(self)

    def padLeft(to: Int, padding: String = " ") = {
      val toadd = to - self.length
      if (toadd > 0) ((padding * toadd) + self).takeRight(to) else self
    }

    def padRight(to: Int, padding: String = " ") = {
      val toadd = to - self.length
      if (toadd > 0) (self + (padding * toadd)).take(to) else self
    }

    /**
     * Add newlines to a string such that each line is `width` or less.  Line
     * splits happen only on whitespace.  In the case where a single 'word'
     * is longer than `width`, the newline will simply be inserted after the
     * word.
     */
    def wrapToLines(width: Int = 80): Vector[String] = {
      self.split("\n").toVector.flatMap { line =>
        val (completeLines, lastLine) =
          line.split("\\s+").foldLeft((Vector[String](), "")) {
            case ((lines, currLine), tok) =>
              if (currLine.size + tok.size + 1 > width)
                (lines :+ currLine, tok)
              else if (currLine.isEmpty)
                (lines, tok)
              else
                (lines, currLine + " " + tok)
          }
        completeLines :+ lastLine
      }
      //lines.map(s => f"$s%-80s|").mkString("\n")
    }

    /**
     * Add newlines to a string such that each line is `width` or less.  Line
     * splits happen only on whitespace.  In the case where a single 'word'
     * is longer than `width`, the newline will simply be inserted after the
     * word.
     */
    def wrap(width: Int = 80): String = {
      this.wrapToLines(width).mkString("\n")
    }

    /**
     * Indent the left side of each line by the given number of spaces.
     */
    def indent(spaces: Int): String = {
      indent(" " * spaces)
    }

    /**
     * Append the given string to the left side of each line.
     */
    def indent(leftColumn: String): String = {
      self.split("\n").map(leftColumn + _).mkString("\n")
    }

  }

  def sideBySideStrings(spaceBuffer: Int, columns: String*) = {
    sideBySide(spaceBuffer, columns.map(_.split("\n").toVector): _*).mkString("\n")
  }

  def sideBySide(spaceBuffer: Int, columns: Vector[String]*) = {
    val maxHeight = columns.map(_.size).max
    val vertBuffered =
      for (c <- columns) yield {
        c ++ Vector.fill(maxHeight - c.size)("")
      }
    val horizBuffered =
      (for (c <- vertBuffered.dropRight(1)) yield {
        val maxLineLength = c.map(_.length).max
        for (line <- c) yield {
          line + (" " * (maxLineLength - line.length))
        }
      }) :+ vertBuffered.last
    for (columnLines <- horizBuffered.transpose) yield {
      columnLines.mkString(" " * spaceBuffer)
    }
  }

  private class RegexMatcherSplitIterator(str: String, pattern: String, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter) extends Iterator[(Int, Int)] {
    val m = pattern.r.pattern.matcher(str)
    var prevE: Int = 0
    var queued: Option[(Int, Int)] = None
    var nextE: Option[Int] = Some(0)

    def hasNext() =
      if (queued.isDefined) {
        true
      }
      else if (m.find()) {
        queued =
          if (keepDelimiter == KeepDelimiter.KeepDelimiterAsLast)
            Some(prevE -> m.end)
          else
            Some(prevE -> m.start)
        nextE =
          if (keepDelimiter == KeepDelimiter.KeepDelimiterAsFirst)
            Some(m.start)
          else
            Some(m.end)
        true
      }
      else if (nextE.isDefined) {
        queued = Some(nextE.get -> str.length())
        nextE = None
        true
      }
      else {
        false
      }

    def next() =
      if (hasNext) {
        val n = queued.get
        prevE = nextE.getOrElse(-1)
        queued = None
        n
      }
      else
        Iterator().next()

    override def toString = s"RegexMatcherSplitIterator(string=$str, pattern=$pattern, keepDelimiter=$keepDelimiter, prevE=$prevE, queued=$queued, nextE=$nextE, hasNext=$hasNext)"
  }

  implicit class EnrichedRegex(val self: Regex) extends AnyVal {
    def matches(s: String): Boolean = self.pattern.matcher(s).matches
    def apply(s: String) = groups(s)
    def groups(s: String) = groupsOption(s).getOrElse(sys.error(self.pattern.matcher(s).group))
    def groupsOption(s: String) = self.unapplySeq(s)
    def firstGroup(s: String) = self.findFirstMatchIn(s).map(_.subgroups)
    def allGroups(s: String) = self.findAllMatchIn(s).map(_.subgroups)
  }

}
