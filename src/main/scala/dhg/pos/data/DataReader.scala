package dhg.pos.data;

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.StringUtil._

trait RawDataReader {
  def readRaw(filepath: String): Iterator[Vector[String]]
}

class FileRawDataReader extends RawDataReader {
  def readRaw(filepath: String): Iterator[Vector[String]] = {
    File(filepath).readLines.map(_.splitWhitespace)
  }
}

class RecursiveTxtFileRawDataReader extends RawDataReader {
  def readRaw(filepath: String): Iterator[Vector[String]] = {
    File(filepath).ls("+*\\.txt".r).iterator.flatMap(_.readLines).map(_.splitWhitespace)
  }
}

//

trait TaggedDataReader[Tag] {
  def readTagged(filepath: String): Iterator[Vector[(String, Tag)]]
}

class FileTaggedDataReader extends TaggedDataReader[String] {
  type Tag = String
  def readTagged(filepath: String): Iterator[Vector[(String, Tag)]] = {
    File(filepath).readLines.map(_.splitWhitespace.map(_.rsplit("\\|").toTuple2))
  }
}

class RecursiveTxtFileTaggedDataReader[Tag] extends TaggedDataReader[String] {
  type Tag = String
  def readTagged(filepath: String): Iterator[Vector[(String, Tag)]] = {
    File(filepath).ls("+*\\.txt".r).iterator.flatMap(_.readLines).map(_.splitWhitespace.map(_.rsplit("\\|").toTuple2))
  }
}
