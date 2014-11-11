package dhg.util

import scala.sys.process._
import java.io.PrintStream
import scala.collection.generic.Growable
import java.io.Writer
import dhg.util.Subprocess._

/**
 * A class for conveniently running command-line operations.
 *
 * e.g.
 *   val cmd = Subprocess.findBinary("tr")
 *   cmd.args("l", "L").call("hello")   // "heLLo"
 *   
 *   val ls = Subprocess("ls")
 *   ls(".").call()
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class Subprocess(binary: String, args: Seq[String]) {

  /**
   * Create a callable subprocess object
   *
   * @param args		A list of command-line arguments.
   * @return new Subprocess
   */
  def args(args: String*) = new Subprocess(binary, args)

  /**
   * Create a callable subprocess object
   *
   * @param newArgs		A list of command-line arguments to be appended to the end of the existing arg list.
   * @return new Subprocess
   */
  def appendArgs(newArgs: String*) = new Subprocess(binary, args ++ newArgs)
  
  /**
   * Alias for `appendArgs`
   */
  def apply(newArgs: String*) = new Subprocess(binary, args ++ newArgs)

  def base() = new Subprocess(binary, Nil)
  def noargs() = this.base

  /**
   * Call the binary
   *
   * @return stdout
   */
  def call(): String = {
    val (exitcode, stdout, stderr) = callAllReturns()
    if (exitcode != 0)
      sys.error(s"ERROR CALLING: $binary ${args.mkString(" ")}\nReturncode: $exitcode\n$stderr")
    stdout
  }

  /**
   * Call the binary with the given input
   *
   * @param input	A string whose contents are used as stdin
   * @return stdout
   */
  def call(inputStr: String): String = {
    val (exitcode, stdout, stderr) = callAllReturns(inputStr)
    if (exitcode != 0)
      sys.error(s"ERROR CALLING: $binary ${args.mkString(" ")}\nReturncode: $exitcode\n$stderr")
    stdout
  }

  /**
   * Call the binary
   *
   * @return (returncode, stdout, stderr)
   */
  def callAllReturns(): (Int, String, String) = {
    val out = new StringBuilder
    val err = new StringBuilder
    val exitcode = callWithStreams(out, err)
    (exitcode, out.result, err.result)
  }

  /**
   * Call the binary with the given input
   *
   * @param input	A string whose contents are used as stdin
   * @return (returncode, stdout, stderr)
   */
  def callAllReturns(input: String): (Int, String, String) = {
    val out = new StringBuilder
    val err = new StringBuilder
    val exitcode = callWithStreams(input, out, err)
    (exitcode, out.result, err.result)
  }

  /**
   * Call the binary and give objects to which the outputs will be streamed.
   *
   * @param out		an appendable object where stdout information is written
   * @param err		an appendable object where stderr information is written
   * @return exitcode
   */
  def callWithStreams[T, R](out: Appendable[T, String], err: Appendable[R, String]): Int = {
    Process(binary +: args) ! ProcessLogger(out.append(_).append("\n"), err.append(_).append("\n"))
  }

  /**
   * Call the binary with the given input and give objects to which the
   * outputs will be streamed.
   *
   * @param input	A string whose contents are used as stdin
   * @param out		an appendable object where stdout information is written
   * @param err		an appendable object where stderr information is written
   * @return exitcode
   */
  def callWithStreams[T, R](input: String, out: Appendable[T, String], err: Appendable[R, String]): Int = {
    Process(List("echo", input)) #| Process(binary +: args) ! ProcessLogger(out.append(_).append("\n"), err.append(_).append("\n"))
  }
}

object Subprocess {

  def apply(binary: String, args: String*) = new Subprocess(binary, args)
  
  /**
   * Trait for Appendable classes that can serve as `out` or `err` streams.
   */
  trait Appendable[T, A] {
    def self: T
    def append(a: A): this.type
  }

  implicit class AppendableStringBuilder(val self: StringBuilder) extends Appendable[StringBuilder, String] {
    def append(a: String) = { self.append(a); this }
  }

  implicit class AppendableGrowable[A, T <: Growable[A]](val self: T) extends Appendable[T, A] {
    def append(a: A) = { self += a; this }
  }

  implicit class AppendableWriter[T <: Writer](val self: T) extends Appendable[T, String] {
    def append(a: String) = { self.write(a); this }
  }

  implicit class AppendablePrintStream[A](val self: PrintStream) extends Appendable[PrintStream, A] {
    def append(a: A) = { self.print(a); this }
  }

  /**
   * Look up a binary and create a Subprocess object for it.
   */
  def findBinary(binaryName: String, binDir: Option[String] = None, envar: Option[String] = None) =
    new Subprocess(FileUtil.findBinary(binaryName, binDir, envar), Nil)
}
