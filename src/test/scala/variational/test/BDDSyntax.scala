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
class BDDSyntax extends FunSuite with ShouldMatchers  {
  def testParse(text : String)(expected : BDD) {
    test(text) {
      BDD.fromString(text) should equal (expected)
    }
  }

  def testParseResource(resource : String) {
    test(resource) {
      val url = getClass.getResource(resource)
      if (url == null)
        assert(false, "unknown resource: " + resource)
      else
        print(BDD.fromURL(url))
    }
  }

  testParse("foo") {
    BDD(0)
  }

  testParse("!foo") {
    !BDD(0)
  }

  testParse("! foo") {
    !BDD(0)
  }

  testParse("~foo") {
    !BDD(0)
  }

  testParse("~ foo") {
    !BDD(0)
  }

  testParse("not foo") {
    !BDD(0)
  }

  testParse("foo&bar") {
    BDD(0) && BDD(1)
  }

  testParse("foo & bar") {
    BDD(0) && BDD(1)
  }

  testParse("foo&&bar") {
    BDD(0) && BDD(1)
  }

  testParse("foo && bar") {
    BDD(0) && BDD(1)
  }

  testParse("foo and bar") {
    BDD(0) && BDD(1)
  }

  testParse("foo|bar") {
    BDD(0) || BDD(1)
  }

  testParse("foo | bar") {
    BDD(0) || BDD(1)
  }

  testParse("foo||bar") {
    BDD(0) || BDD(1)
  }

  testParse("foo || bar") {
    BDD(0) || BDD(1)
  }

  testParse("foo or bar") {
    BDD(0) || BDD(1)
  }

  testParse("A || !A") {
    BDD(true)
  }

  testParse("(A -> B) <=> (!B -> !A)") {
    BDD(true)
  }

  testParse("((!A -> B) && (!A -> !B)) -> A") {
    BDD(true)
  }

  testParse("!(A && B) <=> (!A || !B)") {
    BDD(true)
  }

  testParse("((A -> B) && (B -> C)) -> (A -> C)") {
    BDD(true)
  }

  testParse("((A || B) && (A -> C) && (B -> C)) -> C") {
    BDD(true)
  }

  testParseResource("medium.formula")

}