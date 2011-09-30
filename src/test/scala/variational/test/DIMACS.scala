package variational.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import variational.BDD

/**
 * Tests for BDD.parseDIMACS
 *
 * @author Tillmann Rendel
 */

class DIMACS extends FunSuite with ShouldMatchers {
  for (n <- 1 to 50) {
    val name = "uf20-0" + n + ".cnf"
    val url = getClass.getResource(name)
    test(name) {
      if (url == null)
        assert(false, "resource not found")
      else
        BDD.parseDIMACS(url) should not equal (BDD.Low)
    }
  }
}