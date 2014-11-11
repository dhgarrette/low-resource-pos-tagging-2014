package dhg.util

import java.io.File
import java.io.Reader
import java.io.Writer
import scala.io.Source
import java.io.Closeable
import java.io.BufferedReader

/**
 * Automatic Resource Management (ARM) utility.
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object Arm {

  trait Managed[T] {
    def self: T
    def close(): Unit
  }

  implicit class ManagedCloseable[T <: Closeable](val self: T) extends Managed[T] {
    def close() { self.close() }
  }

  implicit class ManagedSource[T <: Source](val self: T) extends Managed[T] {
    def close() { self.close() }
  }

  /**
   * Automatic Resource Management.  Ensure that the resource is closed after
   * executing the block.
   *
   * Example:
   *   using(new BufferedReader(new FileReader("file"))) { r =>
   *     var count = 0
   *     while (r.readLine != null) count += 1
   *     println(count)
   *   }
   */
  def using[T, R](resource: Managed[T])(block: T => R): R = {
    try {
      block(resource.self)
    }
    finally {
      resource.close()
    }
  }

}
