package dhg.util

import scala.collection.generic.CanBuildFrom

/**
 * Pattern-matching utilities
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object Pattern {

  /**
   * Make it possible to do:
   *   val UInt(x) = "-15"
   */
  object UInt {
    val IntRE = """^(-?\d+)$""".r
    def unapply(v: String): Option[Int] = v match {
      case IntRE(s) => Some(s.toInt)
      case _ => None
    }
  }

  /**
   * Make it possible to do:
   *   val UDouble(x) = "-15.0"
   */
  object UDouble {
    val DoubleRE = """^(-?\d+\.?\d*|-?\d*\.?\d+)$""".r
    def unapply(v: String): Option[Double] = v match {
      case DoubleRE(s) => Some(s.toDouble)
      case _ => None
    }
  }

  /**
   * Make it possible to do:
   *   val UBoolean(x) = "true"
   */
  object UBoolean {
    val booleanRE = """([Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee])""".r
    def unapply(v: String): Option[Boolean] = v match {
      case booleanRE(s) => Some(s.toBoolean)
      case _ => None
    }
  }

  /**
   * Make it possible to do:
   *   val a -> b = (1,2)
   */
  object -> {
    def unapply[A, B](pair: (A, B)): Option[(A, B)] = Some(pair)
  }

  /**
   * Make it possible to do:
   *   val Coll(a, b, c @ _*) = Set(1,2,3,4,5)
   *   val Coll(d -> e) = Map(6 -> 'g)
   */
  object Coll {
    def unapplySeq[T](s: Iterable[T]): Option[Vector[T]] = Some(s.toVector)
  }

  /**
   * Make it possible to do:
   *   val SetHeadTail(a, bs) = Set(1,2,3)
   * where `a` is an element and `bs` is the set of remaining elements 
   */
  object SetHeadTail {
    def unapply[T](s: Set[T]): Option[(T, Set[T])] = {
      if (s.isEmpty) None
      else {
        val a +: bs = s.toVector
        Some((a, bs.toSet))
      }
    }
  }

  /**
   * Make it possible to interpret and create range strings:
   *
   *   val aVector = RangeString("1-3, 5, 6, 7-9,11-12, 13-14")
   *   val RangeString(s) = "1-3, 5, 6, 7-9,11-12, 13-14"  // makes the same Vector
   *
   *   val rangeString = RangeString(Seq(1,2,3,5,7,8,9))   // rangeString = "1-3,5,7-9"
   *   val RangeString(rangeString) = Seq(1,2,3,5,7,8,9)   // makes the same String
   */
  object RangeString {
    val RangeRE = """^(\d+)-(\d+)$""".r
    val OpenRangeRE = """^(\d+)-$""".r

    /**
     * Interpret the range string as a sequence of integers
     */
    def apply(s: String): Vector[Int] = {
      s.replaceAll("\\s+", "").split(",").flatMap {
        case UInt(i) => i to i
        case RangeRE(UInt(b), UInt(e)) if b <= e => b to e
      }.toVector
    }

    /**
     * Make a succinct string that describes the given sequence.
     */
    def apply(seq: Seq[Int]): String = {
      assert(seq.nonEmpty, "cannot make empty sequence into a range string")
      (-2 +: seq).sliding(2).foldLeft(Vector[Vector[Int]]()) {
        case (_, Seq(_, b)) if b < 0 =>
          throw new AssertionError(s"negative numbers are not permitted: $seq")
        case ((z :+ c), Seq(a, b)) =>
          if (a != b - 1)
            (z :+ c) :+ Vector(b)
          else
            (z :+ (c :+ b))
        case (z, Seq(a, b)) =>
          z :+ Vector(b)
      }
        .map {
          case Seq(x) => x.toString
          case s => s.head + "-" + s.last
        }.mkString(",")
    }

    def unapply(s: String): Option[Vector[Int]] = Some(apply(s))
    def unapply(seq: Seq[Int]): Option[String] = Some(apply(seq))
  }

  class RangeString(max: Int) {
    /**
     * Interpret the range string as a sequence of integers
     */
    def apply(s: String): Vector[Int] = {
      s.replaceAll("\\s+", "").split(",").flatMap {
        case UInt(i) => i to i
        case RangeString.RangeRE(UInt(b), UInt(e)) if b <= e => b to e
        case RangeString.OpenRangeRE(UInt(b)) => b to max
      }.toVector
    }
    def unapply(s: String): Option[Vector[Int]] = Some(apply(s))
  }

}
