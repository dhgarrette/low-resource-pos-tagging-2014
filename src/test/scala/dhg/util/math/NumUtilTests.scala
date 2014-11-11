package dhg.util.math

import scala.math._
import org.junit.Assert._
import org.junit.Test
import dhg.util.math.NumUtil._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class NumUtilTests {

  @Test
  def test_sum() {
    assertEquals("int", (5 ** 3).getClass.getName)
    assertEquals(125, 5 ** 3)
    assertEquals("double", (5 ** 3.0).getClass.getName)
    assertEquals(125.0, 5 ** 3.0, 0.000001)
    assertEquals("double", (5.0 ** 3).getClass.getName)
    assertEquals(125.0, 5.0 ** 3, 0.000001)
    assertEquals("double", (5.0 ** 3.0).getClass.getName)
    assertEquals(125.0, 5.0 ** 3.0, 0.000001)
  }

}
