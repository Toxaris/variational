package variational.test

import variational._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import Generic.liftGenericVariational

class Tests extends FunSuite with ShouldMatchers {
  test("smart choice 1") {
    val v1 = Choice(0, VariationalLike(1), VariationalLike(2))
    val v2 = VariationalLike(3)

    Choice(1, v1, v2) should equal(Choice(0, Choice(1, VariationalLike(1), VariationalLike(3)), Choice(1, VariationalLike(2), VariationalLike(3))))
  }

  test("mapping over constant values") {
    VariationalLike(21).map(_ + 21) should equal(VariationalLike(42))
  }

  test("mapping over variable values") {
    Choice(0, VariationalLike(1), VariationalLike(2)).map(_ + 1) should equal(Choice(0, VariationalLike(2), VariationalLike(3)))
  }

  test("mapping over variable values -- merging nodes") {
    Choice(0, VariationalLike(1), VariationalLike(2)).map(_ * 0) should equal(VariationalLike(0))
  }

  test("selecting a variable not mentioned (1)") {
    val v = Choice(1, VariationalLike(1), VariationalLike(2))
    v.select(0) should equal(v)
  }

  test("selecting a variable not mentioned (2)") {
    val v = Choice(1, VariationalLike(1), VariationalLike(2))
    v.select(2) should equal(v)
  }

  test("selecting a mentioned variable") {
    val v = Choice(1, VariationalLike(1), VariationalLike(2))
    v.select(1) should equal(VariationalLike(1))
  }

  test("selecting a mentioned variable -- merging") {
    val v = Choice(1, Choice(2, VariationalLike(1), VariationalLike(2)), VariationalLike(1))
    v.select(2) should equal(VariationalLike(1))
  }

  test("deselecting a variable not mentioned (1)") {
    val v = Choice(1, VariationalLike(1), VariationalLike(2))
    v.deselect(0) should equal(v)
  }

  test("deselecting a variable not mentioned (2)") {
    val v = Choice(1, VariationalLike(1), VariationalLike(2))
    v.deselect(2) should equal(v)
  }

  test("deselecting a mentioned variable") {
    val v = Choice(1, VariationalLike(1), VariationalLike(2))
    v.deselect(1) should equal(VariationalLike(2))
  }

  test("deselecting a mentioned variable -- merging") {
    val v = Choice(1, Choice(2, VariationalLike(2), VariationalLike(1)), VariationalLike(1))
    v.deselect(2) should equal(VariationalLike(1))
  }

  test("variability-aware application (1)") {
    val succ = (n : Int) => n + 1
    val twice = (n : Int) => n * 2
    val f = Choice(0, VariationalLike(succ), VariationalLike(twice))
    val a = Choice(0, VariationalLike(5), VariationalLike(10))
    f(a) should equal(Choice(0, VariationalLike(6), VariationalLike(20)))
  }

  test("variability-aware application (2)") {
    val succ = (n : Int) => n + 1
    val twice = (n : Int) => n * 2
    val f = Choice(1, VariationalLike(succ), VariationalLike(twice))
    val a = Choice(0, VariationalLike(1), VariationalLike(5))
    f(a) should equal(Choice(0, VariationalLike(2), Choice(1, VariationalLike(6), VariationalLike(10))))
  }

  test("decomposition") {
      val v = Choice(1, Choice(2, VariationalLike("foo"), VariationalLike("bar")), Choice(3, VariationalLike("foo"), VariationalLike("bar")))
      v.decompose should equal (Choice(1, Choice(2, VariationalLike(0), VariationalLike(1)), Choice(3, VariationalLike(0), VariationalLike(1))), Seq("foo", "bar"))
  }

  test("indexing") {
      val indices = Choice(1, VariationalLike(0), VariationalLike(1))
      val common = Choice(2, VariationalLike("baz"), VariationalLike("boo"))
      val values = Seq(Choice(0, VariationalLike("foo"), VariationalLike("bar")), common)
      VariationalLike.index(values, indices) should equal (Choice(0, Choice(1, VariationalLike("foo"), common), Choice(1, VariationalLike("bar"), common)))
  }

  test("flatMap") {
      val v = Choice(0, VariationalLike('a'), Choice(2, VariationalLike('b'), VariationalLike('B')))

      val result = for {
        letter <- v
        normalized <- Choice(1, VariationalLike(letter.toLower), VariationalLike(letter.toUpper))
      } yield normalized

      result should equal (Choice(0, Choice(1, VariationalLike('a'), VariationalLike('A')), Choice(1, VariationalLike('b'), VariationalLike('B'))))
  }
}