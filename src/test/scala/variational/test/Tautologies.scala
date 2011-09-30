package variational.test

import variational._

import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Tautologies extends org.scalatest.FunSuite with ShouldMatchers {
  val A = BDD(0)
  val B = BDD(1)
  val C = BDD(2)

  def tautology(name : String)(bdd : => BDD) =
    test(name) {
      bdd should equal (BDD(true))
    }

  tautology("tertium non datur") {
    A || !A
  }

  tautology("contraposition") {
    (A -> B) <=> (!B -> !A)
  }

  tautology("reductio ad absurdum") {
    ((!A -> B) && (!A -> !B)) -> A
  }

  tautology("de Morgan") {
    !(A && B) <=> (!A || !B)
  }

  tautology("syllogism") {
    ((A -> B) && (B -> C)) -> (A -> C)
  }

  tautology("cases") {
    ((A || B) && (A -> C) && (B -> C)) -> C
  }
}