package variational.test

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import variational.{Choice, BDD}
import variational.BDD.{Low, High}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests for BDD.fromString.
 *
 * @author Tillmann Rendel
 */

@RunWith(classOf[JUnitRunner])
class BDDOperations extends FunSuite with ShouldMatchers {
  test("BDD(0)") {
    BDD(0) should equal (Choice(0, High, Low))
  }

  test("BDD(true)") {
    BDD(true) should equal (High)
  }

  test("BDD(false)") {
    BDD(false) should equal (Low)
  }

  test("!BDD(0)") {
    (!(BDD(0))) should equal (Choice(0, Low, High))
  }

  test("BDD(0) && BDD(0)") {
    (BDD(0) && BDD(0)) should equal (Choice(0, High, Low))
  }

  test("BDD(0) && BDD(1)") {
    (BDD(0) && BDD(1)) should equal (Choice(0, Choice(1, High, Low), Low))
  }

  test("BDD(0) || BDD(0)") {
    (BDD(0) || BDD(0)) should equal (Choice(0, High, Low))
  }

  test("BDD(0) || BDD(1)") {
    (BDD(0) || BDD(1)) should equal (Choice(0, High, Choice(1, High, Low)))
  }
}