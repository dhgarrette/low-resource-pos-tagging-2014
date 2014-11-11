package dhg.util.math

object NumUtil {

  implicit class EnrichedInt(val self: Int) extends AnyVal {
    def pow(e: Int): Int = (1 until e).foldLeft(self)((z, _) => z * self)
    def **(e: Int): Int = pow(e)
    def pow(e: Double): Double = math.pow(self, e)
    def **(e: Double): Double = math.pow(self, e)

    def toLogDouble: LogDouble = LogDouble(self)
    def log: LogDouble = toLogDouble
  }

  implicit class EnrichedDouble(val self: Double) extends AnyVal {
    def pow(e: Double): Double = math.pow(self, e)
    def **(e: Double): Double = math.pow(self, e)

    def toLogDouble: LogDouble = LogDouble(self)
    def log: LogDouble = toLogDouble
  }

  //  implicit class NumericWithToLogDouble[N](self: N)(implicit num: Numeric[N]) {
  //    def toLogDouble = LogDouble(self)
  //    def log = toLogDouble
  //  }

}
