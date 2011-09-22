package variational.test

import variational._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class Tautologies extends FunSuite with ShouldMatchers {
  import BDD.{BDD, pimpBDD}

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