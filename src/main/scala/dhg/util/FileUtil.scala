package dhg.util

import java.io.BufferedWriter
import java.io.File
import java.io.File.createTempFile
import java.io.File.separator
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.OutputStreamWriter
import java.io.Writer
import java.net.URI
import scala.collection.breakOut
import scala.io.BufferedSource
import scala.io.Source
import dhg.util.Arm._
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import scala.util.matching.Regex

/**
 * File utilities and enhancement methods for Files
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object FileUtil {

  type File = java.io.File
  
  object File {
    def apply(parent: File, child: String) = new File(parent, child)
    def apply(path: String) = new File(path)
    def apply(path: String*) = new File(pathjoin(path))
    def apply(uri: URI) = new File(uri)
  }

  def file(parent: File, child: String) = new File(parent, child)
  def file(path: String) = new File(path)
  def file(path: String*) = new File(pathjoin(path))
  def file(uri: URI) = new File(uri)

  def pathjoin(part: String, parts: String*): String = {
    pathjoin(part +: parts)
  }

  def pathjoin(parts: Seq[String]): String = {
    val start = if (parts.head.startsWith(separator)) separator else ""
    parts.flatMap(_.split(separator)).filter(_.nonEmpty).mkString(start, separator, "")
  }

  /**
   * Generate a temporary filename but do not actually create the file in the
   * filesystem.
   */
  def mktemp(prefix: String = "temp-", suffix: String = ""): File = {
    val f = createTempFile(prefix, suffix)
    f.delete()
    f
  }

  implicit class EnhancedFile(val self: File) extends AnyVal {

    def path = {
      self.getPath
    }

    def name = {
      self.getName
    }

    def parent = {
      Option(self.getParentFile)
    }

    /**
     * Separate the filename from the parent directory.
     * Return Some(parentDir, filename) if there is a parent directory,
     * and None otherwise.
     */
    def parentFilename = {
      (self.parent, self.name)
    }

    /**
     * Get the path as a sequence of strings.
     */
    def pathSeq: Vector[String] = {
      val (parent, file) = parentFilename
      parent.map(_.pathSeq).getOrElse(Vector()) :+ file
    }

    /**
     * Creating the file's containing directory structure if necessary.
     */
    def mkParentDir() {
      self.parent.foreach(_.mkdirs())
    }

    def ls(): Vector[File] = {
      assert(self.exists, s"'$self' does not exist")
      assert(self.isDirectory, s"'$self' is not a directory")
      self.listFiles.toVector
    }

    def ls(regex: Regex, pathMatch: Boolean = false): Vector[File] = {
      assert(self.exists, s"'$self' does not exist")
      assert(self.isDirectory, s"'$self' is not a directory")
      val files = self.listFiles.toVector
      def getName(f: File) =
        if (pathMatch) f.getAbsolutePath
        else f.getName
      files.filter(f => regex.pattern.matcher(getName(f)).matches)
    }

    /**
     * List all files (but not directories), searching recursively through sub-directories.
     */
    def listFilesRecursive(): Vector[File] = {
      assert(self.exists, s"'$self' does not exist")
      assert(self.isDirectory, s"'$self' is not a directory")
      self.ls.flatMap { f =>
        if (f.isDirectory)
          f.listFilesRecursive
        else
          Vector(f)
      }
    }

    /**
     * List all files (but not directories), searching recursively through sub-directories.
     */
    def listFilesRecursive(regex: Regex, pathMatch: Boolean = false): Vector[File] = {
      assert(self.exists, s"'$self' does not exist")
      assert(self.isDirectory, s"'$self' is not a directory")
      self.ls(regex, pathMatch).flatMap { f =>
        if (f.isDirectory)
          f.listFilesRecursive
        else
          Vector(f)
      }
    }

    /**
     * Return a path to this relative to the given directory.
     */
    def relativeTo(dir: File): Option[File] = {
      val dirPath = dir.getAbsolutePath
      val selfPath = self.getAbsolutePath
      if (selfPath.startsWith(dirPath)) {
        val a = selfPath.drop(dirPath.length)
        val s = if (a.startsWith(separator)) a.drop(1) else a
        Some(File(s))
      }
      else
        None
    }

    /**
     * Read the contents of this file, making sure to close the file after all
     * lines have been read.
     */
    def readLines: Iterator[String] = {
      readLines("UTF-8")
    }

    /**
     * Read the contents of this file, making sure to close the file after all
     * lines have been read.
     */
    def readLines(encoding: String): Iterator[String] = {
      SelfClosingBufferedReaderIterator(bufferedReader(self, encoding))
    }

  }

  def bufferedReader(file: File, encoding: String = "UTF-8") = {
    new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding))
  }

  /**
   * Get an Iterator over the lines in the BufferedReader.
   */
  case class BufferedReaderIterator(reader: BufferedReader) extends Iterator[String] {
    override def hasNext() = reader.ready
    override def next() = reader.readLine()
  }

  /**
   * Get a BufferedReader for GZIP files
   */
  object GzFileBufferedReader {
    def apply(file: File, encoding: String = "UTF-8"): BufferedReader = {
      new BufferedReader(
        new InputStreamReader(
          new GZIPInputStream(
            new FileInputStream(file)), encoding))
    }
  }

  /**
   * Iterator over the lines in the BufferedReader.  The reader will
   * automatically close itself when the end is reached.  This gets around the
   * problem of having to all of your processing inside the `using` block.
   */
  case class SelfClosingBufferedReaderIterator(bufferedReader: BufferedReader) extends Iterator[String] {
    private[this] val blockItr = BufferedReaderIterator(bufferedReader)
    private[this] var finished = false
    override def next() = {
      hasNext()
      if (finished) throw new NoSuchElementException("next on empty iterator")
      val n = blockItr.next
      hasNext()
      n
    }
    override def hasNext() = {
      if (finished)
        false
      else {
        val hn = blockItr.hasNext
        if (!hn) {
          finished = true
          bufferedReader.close()
        }
        hn
      }
    }
  }

  def bufferedWriter(file: File, encoding: String = "UTF-8") = {
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), encoding))
  }

  def findBinary(name: String, binDir: Option[String] = None, envar: Option[String] = None): String = {
    val checked = collection.mutable.Buffer[String]()

    for (d <- binDir) {
      val path = pathjoin(d, name)
      if (File(path).exists)
        return path
      else
        checked += path
    }

    for (ev <- envar; envpath <- Option(System.getenv(ev))) {
      val path = envpath + "/" + name
      if (File(path).exists)
        return path
      else
        checked += path
    }

    try {
      val found = scala.sys.process.Process(List("which", name)).!!
      return found.trim
    }
    catch {
      case _: Throwable => checked += s"which $name"
    }

    throw new RuntimeException("No binary found.  Checked the following:\n" + checked.map((" ") * 16 + _).mkString("\n"))
  }

  /**
   * Open a file for reading, execute a block of code, and ensure that the
   * file is closed when finished.
   */
  def readUsing[R](file: File, encoding: String = "UTF-8")(block: BufferedSource => R): R = {
    using(Source.fromFile(file, encoding))(block)
  }

  /**
   * Open a file for writing (creating the containing directory structure if
   * necessary), execute a block of code, and ensure that the file is closed
   * when finished.
   */
  def writeUsing[R](file: File, encoding: String = "UTF-8")(block: BufferedWriter => R): R = {
    file.mkParentDir()
    using(bufferedWriter(file, encoding))(block)
  }

  /**
   * Add a method `writeLine` to Writer classes
   */
  implicit class WriterWithWriteLine(val self: Writer) extends AnyVal {
    def writeLine(line: Any) { self.write(line + "\n") }
    def writeLine() { self.write("\n") }

    def wl(line: Any) = writeLine(line)
    def wl() = writeLine()
  }

}
