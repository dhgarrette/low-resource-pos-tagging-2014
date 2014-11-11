package dhg.util

import org.junit.Assert._
import org.junit.Test

import dhg.util.FileUtil._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class FileUtilTests {

  @Test
  def test_pathjoin() {

    assertEquals("/a/b/c", pathjoin("/a/b", "c"))
    assertEquals("/a/b/c", pathjoin("/a/b/", "c"))
    assertEquals("/a/b/c", pathjoin("/a", "b/c"))
    assertEquals("/a/b/c", pathjoin("/a", "/b/c"))
    assertEquals("/a/b/c", pathjoin("/a/", "b/c"))
    assertEquals("/a/b/c", pathjoin("/a/", "/b/c"))

    assertEquals("a/b/c", pathjoin("a/b", "c"))
    assertEquals("a/b/c", pathjoin("a/b/", "c"))
    assertEquals("a/b/c", pathjoin("a", "b/c"))
    assertEquals("a/b/c", pathjoin("a", "/b/c"))
    assertEquals("a/b/c", pathjoin("a/", "b/c"))
    assertEquals("a/b/c", pathjoin("a/", "/b/c"))
    
  File("a/b/c/d").relativeTo(File("a/b"))         //> res0: Option[java.io.File] = Some(c/d)
  File("a/b/c/d").relativeTo(File("a"))           //> res1: Option[java.io.File] = Some(b/c/d)
  File("a/b/c/d").relativeTo(File("a/b/c/d"))     //> res2: Option[java.io.File] = Some()
  File("a/b/c/d").relativeTo(File("/a/b"))        //> res3: Option[java.io.File] = None
  File("a/b/c/d").relativeTo(File(""))            //> res4: Option[java.io.File] = Some(a/b/c/d)
  File("a/b/c/d").relativeTo(File("/"))           //> res5: Option[java.io.File] = Some(Applications/eclipse-3.7-2.10/Eclipse.app/
                                                  //| Contents/MacOS/a/b/c/d)
  File("/a/b/c/d").relativeTo(File(""))           //> res6: Option[java.io.File] = None
  File("/a/b/c/d").relativeTo(File("/a"))         //> res7: Option[java.io.File] = Some(b/c/d)

  File("a").getParent                             //> res8: String = null
  File("a").parent                                //> res9: Option[java.io.File] = None
  File("a/b").getParent                           //> res10: String = a
  File("a/b").parent                              //> res11: Option[java.io.File] = Some(a)


  }

}
