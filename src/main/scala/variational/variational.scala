package variational

abstract class Case[A, B]

object Case {
  case class Left[A, B](condition : Int, thenBranch : V[A], elseBranch : V[A]) extends Case[A, B]
  case class Right[A, B](condition : Int, thenBranch : V[B], elseBranch : V[B]) extends Case[A, B]
  case class Both[A, B](condition : Int, thenBranch1 : V[A], elseBranch1 : V[A], thenBranch2 : V[B], elseBranch2 : V[B]) extends Case[A, B]
  case class Simple[A, B](value1 : A, value2 : B) extends Case[A, B]

  def apply[A, B](variational1 : V[A], variational2 : V[B]) : Case[A, B] =
    variational1 match {
      case Constant(value1) => variational2 match {
        case Constant(value2) => Simple(value1, value2)
        case Choice(condition2, thenBranch2, elseBranch2) => Right(condition2, thenBranch2, elseBranch2)
      }
      case Choice(condition1, thenBranch1, elseBranch1) => variational2 match {
        case Constant(value2) => Left(condition1, thenBranch1, elseBranch1)
        case Choice(condition2, thenBranch2, elseBranch2) =>
          if (condition1 < condition2)
            Left(condition1, thenBranch1, elseBranch1)
          else if (condition2 < condition1)
            Right(condition2, thenBranch2, elseBranch2)
          else
            Both(condition1, thenBranch1, elseBranch1, thenBranch2, elseBranch2)
      }
    }
}

abstract class V[A] {
  import scala.collection.mutable
  import mutable.WeakHashMap

  def map[B](f : A => B) : V[B] =
    map(f, scala.collection.mutable.Map())

  def map[B](f : A => B, cache : mutable.Map[A, V[B]]) : V[B]

  def select(variable : Int) : V[A]

  def deselect(variable : Int) : V[A]

  def zipWith[B, C](f : (A, B) => C, that : V[B]) : V[C] = {
    def worker(variational1 : V[A], variational2 : V[B]) : V[C] =
        Case(variational1, variational2) match {
          case Case.Left(condition, thenBranch1, elseBranch1) =>
            Choice(condition, worker(thenBranch1, variational2), worker(elseBranch1, variational2))
          case Case.Right(condition, thenBranch2, elseBranch2) =>
            Choice(condition, worker(variational1, thenBranch2), worker(variational1, elseBranch2))
          case Case.Both(condition, thenBranch1, elseBranch1, thenBranch2, elseBranch2) =>
            Choice(condition, worker(thenBranch1, thenBranch2), worker(elseBranch1, elseBranch2))
          case Case.Simple(value1, value2) =>
            V(f(value1, value2))
        }

    worker(this, that)
  }

  def zip[B](that : V[B]) : V[(A, B)] =
    zipWith((a : A, b : B) => (a, b), that)

  def decompose : (V[Int], Seq[A]) = {
    val cache = scala.collection.mutable.Map[A, Int]()
    val builder = Seq.newBuilder[A]
    var nextIndex = 0

    val indices = for {
      element <- this
    } yield {
      cache.getOrElseUpdate(element, {
        val index = nextIndex
        nextIndex += 1
        builder += element
        index
      })
    }

    (indices, builder.result)
  }

  def flatMap[B](f : A => V[B]) : V[B] =
    decompose match {
      case (indices, values) => V.index(values.map(f), indices)
    }

  private[this]
  val cache : WeakHashMap[Int, WeakHashMap[V[A], V[A]]] = WeakHashMap()

  def choice(variable : Int, that : V[A])(implicit merge : (A, A) => Option[A] = (x : A, y : A) => None) : V[A] = {
    if (this == that)
      this
    else
      cache.
      getOrElseUpdate(variable, WeakHashMap()).
      getOrElseUpdate(that, Case(this, that) match {
        case Case.Left(condition, thenBranch, elseBranch) =>
          if (variable < condition)
            new Choice(variable, this, that)
          else if (variable == condition)
            new Choice(variable, thenBranch, that)
          else
            new Choice(condition, thenBranch.choice(variable, that), elseBranch.choice(variable, that))
        case Case.Right(condition, thenBranch, elseBranch) =>
          if (variable < condition)
            new Choice(variable, this, that)
          else if (variable == condition)
            new Choice(variable, this, elseBranch)
          else
            new Choice(condition, this.choice(variable, thenBranch), this.choice(variable, elseBranch))
        case Case.Both(condition, thenBranch1, elseBranch1, thenBranch2, elseBranch2) =>
          if (variable < condition)
            new Choice(variable, this, that)
          else if (variable == condition)
            new Choice(variable, thenBranch1, elseBranch2)
          else
            new Choice(condition, thenBranch1.choice(variable, thenBranch2), elseBranch1.choice(variable, elseBranch2))
        case Case.Simple(value1, value2) =>
          merge(value1, value2) match {
            case Some(value) => V(value)
            case None => new Choice(variable, this, that)
          }
      })
  }
}

object V {
  def apply[A](value : A) : V[A] = Constant(value)

  implicit def pimpApply[A, B](function : V[A => B]) = new {
    def apply(argument : V[A]) : V[B] =
      function.zipWith((function : A => B, argument : A) => function(argument), argument)
  }

  def index[A](values : Seq[V[A]], indices : V[Int]) : V[A] =
    indices match {
      case Constant(i) => values(i)
      case Choice(condition, thenBranch, elseBranch) => {
        val variables =
          for {
            value <- values
            variable <- value match {
              case Constant(_) => Seq()
              case Choice(variable, _, _) => Seq(variable)
            }
          } yield variable

        val variable = (condition +: variables).min

        Choice(variable,
          index(values.map(_.select(variable)), indices.select(variable)),
          index(values.map(_.deselect(variable)), indices.deselect(variable)))
      }
    }
}

class Choice[A](val condition : Int, val thenBranch : V[A], val elseBranch : V[A]) extends V[A] {
  import scala.collection.mutable

  def map[B](f : A => B, cache : mutable.Map[A, V[B]]) : V[B] =
    Choice(condition, thenBranch.map(f, cache), elseBranch.map(f, cache))

  def select(variable : Int) : V[A] =
    if (variable < condition)
      this
    else if (variable == condition)
      thenBranch
    else
      Choice(condition, thenBranch.select(variable), elseBranch.select(variable))

  def deselect(variable : Int) : V[A] =
    if (variable < condition)
      this
    else if (variable == condition)
      elseBranch
    else
      Choice(condition, thenBranch.deselect(variable), elseBranch.deselect(variable))

  override def toString() =
    "Choice(" + condition + ", " + thenBranch + ", " + elseBranch + ")"
}

object Choice {
  def apply[A](condition : Int, thenBranch : V[A], elseBranch : V[A]) : V[A] =
    thenBranch.choice(condition, elseBranch)

  def unapply[A](variational : Choice[A]) : Some[(Int, V[A], V[A])] =
    Some(variational.condition, variational.thenBranch, variational.elseBranch)
}

class Constant[A](val value : A) extends V[A] {
  import scala.collection.mutable

  def map[B](f : A => B, cache : mutable.Map[A, V[B]]) =
    cache.getOrElseUpdate(value, Constant(f(value)))

  def select(variable : Int) : V[A] =
    this

  def deselect(variable : Int) : V[A] =
    this

  override def toString() =
    "Constant(" + value + ")"
}

object Constant {
  import scala.collection.mutable.WeakHashMap

  type Cache[A] = WeakHashMap[A, Constant[A]]

  val cache : Cache[Any] = WeakHashMap()

  def apply[A](value : A) : Constant[A] =
    cache.asInstanceOf[Cache[A]].getOrElseUpdate(value, new Constant(value))

  def unapply[A](variational : Constant[A]) : Some[A] =
    Some(variational.value)
}