package dhg.util

/**
 * Enhancement methods for numbers
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object NumberUtil {

  implicit class Enriched_Int(val self: Int) extends AnyVal {

    /**
     * Shorthand for a range from this Int to the max integer value.
     */
    def up: Range = self to Int.MaxValue
    def upi: Iterator[Int] = Iterator.from(self)

    /**
     * Shorthand for a range from this to n by -1
     */
    def downto(n: Int): Range = self to n by -1
  }

  /**
   * A mutable number-holding object
   */
  class MutableNumber[N](private[this] var i: N)(implicit num: Numeric[N]) {
    def this()(implicit num: Numeric[N]) = this(num.zero)
    def +=(o: N) = { i = num.plus(i, o); this }
    def get = i
  }

}
