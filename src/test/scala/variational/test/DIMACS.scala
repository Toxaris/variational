package variational.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import variational.BDD
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests for BDD.parseDIMACS
 *
 * @author Tillmann Rendel
 */

@RunWith(classOf[JUnitRunner])
class DIMACS extends FunSuite with ShouldMatchers {
  // val runtime = Runtime.getRuntime()

  for (n <- 1 to 50) {
    val name = "uf20-0" + n + ".cnf"
    val url = getClass.getResource(name)
    test(name) {
      if (url == null)
        assert(false, "resource not found")
      else
        BDD.parseDIMACS(url) should not equal (BDD.Low)

      // runtime.gc()
      // println("used memory = " + (runtime.totalMemory() - runtime.freeMemory()))
    }

  }
}