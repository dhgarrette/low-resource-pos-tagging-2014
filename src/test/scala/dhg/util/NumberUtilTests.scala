package dhg.util

import scala.collection.mutable.Buffer
import org.junit.Assert._
import org.junit.Test
import dhg.util.CollectionUtil._
import dhg.util.Collections._
import dhg.util.TestUtil._
import dhg.util.NumberUtil._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class NumberUtilTests {

  @Test
  def test_Int_up() {
    assertEquals(4 to Int.MaxValue, 4.up)
  }

  @Test
  def test_Int_downto() {
    assertEquals(5 to 2 by -1, 5 downto 2)
  }

}
