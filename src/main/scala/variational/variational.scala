package variational

import collection.mutable.WeakHashMap

abstract class Lift[A, VA] {
  def apply(value : A) : VA
}

abstract class Variational {
  /**
   * The type of the values (for, e.g., mapping)
   */
  type Value

  /**
   * The type of sub-trees below choice nodes
   */
  type This <: Variational

  /**
   * The least variable this variational value depends on.
   */
  def getCondition : Option[Int]

  /**
   * The top-level structure of this value, if any.
   */
  def getStructure : Option[Class[_]]

  /**
   * The constant variant this value describes.
   */
  def getValue : Option[Value]

  /**
   * Exposes the next choice.
   */
  def exposeChoice : Option[(Int, This, This)]


  def all(f : VFunction1) : This

  def all2(f : VFunction2, that : This) : Option[This]

  /**
   * Constructs a choice node.
   *
   * <p><b>Call</b> this method only when you are sure that such
   * a choice node is not cached. When in doubt, call
   * <code>makeChoice</code> instead.
   *
   * <p><b>Implement</b> this method in concrete subclasses to
   * create the appropriate choice node for your data structure.
   */
  def newChoice(condition : Int, that : This) : This

  /**
   * Reuse an existing choice node. If no such choice node is
   * cached, return and cache the third argument.
   */
  def getChoiceOrElseUpdate(condition : Int, that : This, choice : => This) : This =
    cache.getOrElseUpdate(condition, WeakHashMap()).getOrElseUpdate(that, choice)

  /**
   * Construct or reuse a choice node.
   */
  def makeChoice(condition : Int, that : This) : This =
    getChoiceOrElseUpdate(condition, that, newChoice(condition, that))

  def select(variable : Int) : This

  def deselect(variable : Int) : This

  private[this]
  val cache : WeakHashMap[Int, WeakHashMap[This, This]] = WeakHashMap()

  /**
   * Construct a choice node and reinstate the invariants.
   */
  def smartChoice(variable : Int, that : This) : This

  def map[B, VB <: VariationalLike[VB]](f : Value => B)(implicit lift : Lift[B, VB]) : VB

  // def flatMap[VB <: VariationalLike[VB]](f : A => VB) : VB

  def zipWith[B, C, VC <: VariationalLike[VC]](f : (Value, B) => C, that : Container[B])(implicit lift : Lift[C, VC]) : VC
}

/**
 * An interface for variation programming. All variation-aware
 * values should mix in this trait. It supports operations on
 * the variability structure, independent of the type of the
 * individual variants.
 */
abstract class VariationalLike[Self <: Variational with VariationalLike[Self]] extends Variational {
  this : Self =>

  type This = Self

  /**
   * Construct a choice node and reinstate the invariants.
   */
  def smartChoice(variable : Int, that : This) : This =
    new SmartChoice(variable).apply(this, that)
}

/**
 * Operations for variability-aware containers.
 */
trait Container[A] extends Variational {
  type This <: Container[A]
  type Value = A
}

object Container {
  implicit def pimpApply[A, B](function : Container[A => B]) = new {
    def apply[VB <: VariationalLike[VB]](argument : Container[A])(implicit lift : Lift[B, VB]) : VB =
      function.zipWith((function : A => B, argument : A) => function(argument), argument)
  }
}

trait VariationalContainer[A, VA <: VariationalContainer[A, VA]] extends VariationalLike[VA] with Container[A] {
  this : VA =>

  def zipWith[B, C, VC <: VariationalLike[VC]](f : (A, B) => C, that : Container[B])(implicit lift : Lift[C, VC]) =
    (this, that) match {
      case VariationalLike.Dependent(condition) => {
        val thenBranch : VC = this.select(condition).zipWith(f, that.select(condition))
        val elseBranch : VC = this.deselect(condition).zipWith(f, that.deselect(condition))
        thenBranch.smartChoice(condition, elseBranch)
      }
      case VariationalLike.Constant() =>
        lift(f(this.getValue.get, that.getValue.get))
    }
}

/*
def map[B, VB <: Container[B, VB]](f : A => B)(implicit lifting : Lifting[B, VB]) : VB

def flatMap[VB <: VariationalLike[VB]](f : A => VB) : VB
*/

/*
def zipWith[B <: VariationalLike[B], C <: VariationalLike[C]](f : (A, B) => C, that : B) : C = {
 def worker(variational1 : VariationalLike[A], variational2 : VariationalLike[B]) : VariationalLike[C] =
   Case(variational1, variational2) match {
     case Case.Left(condition, thenBranch1, elseBranch1) =>
       Choice(condition, worker(thenBranch1, variational2), worker(elseBranch1, variational2))
     case Case.Right(condition, thenBranch2, elseBranch2) =>
       Choice(condition, worker(variational1, thenBranch2), worker(variational1, elseBranch2))
     case Case.Both(condition, thenBranch1, elseBranch1, thenBranch2, elseBranch2) =>
       Choice(condition, worker(thenBranch1, thenBranch2), worker(elseBranch1, elseBranch2))
     case Case.Simple() =>
       VariationalLike(f(variational1, variational2))
   }

 worker(this, that)
}

def zip[B](that : VariationalLike[B]) : VariationalLike[(A, B)] =
 zipWith((a : A, b : B) => (a, b), that)

def decompose : (VariationalLike[Int], Seq[A]) = {
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

def flatMap[B](f : A => VariationalLike[B]) : VariationalLike[B] =
 decompose match {
   case (indices, values) => VariationalLike.index(values.map(f), indices)
 }
*/

abstract class Choice[This <: VariationalLike[This]](val condition : Int, val thenBranch : This, val elseBranch : This) extends VariationalLike[This] {
  this : This =>

  lazy val sameChoice = new SmartChoice(condition)

  def getCondition = Some(condition)

  def getStructure = None

  def getValue = None

  def exposeChoice = Some(condition, thenBranch, elseBranch)

  def all(f : VFunction1) =
    sameChoice(thenBranch.all(f), elseBranch.all(f))

  def all2(f : VFunction2, that : This) =
    None

  def select(variable : Int) =
    if (variable < condition)
      this
    else if (variable == condition)
      thenBranch
    else
      sameChoice(thenBranch.select(variable), elseBranch.select(variable))

  def deselect(variable : Int) =
    if (variable < condition)
      this
    else if (variable == condition)
      elseBranch
    else
      sameChoice(thenBranch.deselect(variable), elseBranch.deselect(variable))

  /*
    import scala.collection.mutable

    def map[B](f : A => B, cache : mutable.Map[A, VariationalLike[B]]) : VariationalLike[B] =
      Choice(condition, thenBranch.map(f, cache), elseBranch.map(f, cache))
   */

  override def toString() =
    "Choice(" + condition + ", " + thenBranch + ", " + elseBranch + ")"
}

trait ChoiceContainer[A, VA <: VariationalContainer[A, VA]] extends Choice[VA] with Container[A] {
  this : VA =>
  def map[B, VB <: VariationalLike[VB]](f : (A) => B)(implicit lift : Lift[B, VB]) =
    sameChoice(thenBranch.map(f), elseBranch.map(f))
}

object VariationalLike {
  def apply[A, VA <: VariationalLike[VA]](value : A)(implicit lift : Lift[A, VA]) : VA = lift(value)

  object Equal {
    def unapply(scrutinee : (Variational, Variational)) : Boolean = {
      val scrutinee1 = scrutinee._1
      scrutinee1 == scrutinee._2
    }

    def unapply(scrutinee : (Variational, Variational, Variational)) : Boolean = {
      val scrutinee1 = scrutinee._1
      scrutinee1 == scrutinee._2 && scrutinee1 == scrutinee._3
    }
  }

  object Constant {
    def unapply(scrutinee : (Variational, Variational)) : Boolean =
      scrutinee._1.getCondition.isEmpty && scrutinee._2.getCondition.isEmpty

    def unapply(scrutinee : (Variational, Variational, Variational)) : Boolean =
      scrutinee._1.getCondition.isEmpty && scrutinee._2.getCondition.isEmpty && scrutinee._3.getCondition.isEmpty
  }

  object Structural {
    def unapply(scrutinee : (Variational, Variational)) : Boolean = {
      val (scrutinee1, scrutinee2) = scrutinee
      val class1 = scrutinee1.getClass
      scrutinee1.isInstanceOf[Structure[_]] && class1 == scrutinee2.getClass
    }

    def unapply(scrutinee : (Variational, Variational, Variational)) : Boolean = {
      val (scrutinee1, scrutinee2, scrutinee3) = scrutinee
      val class1 = scrutinee1.getClass
      scrutinee1.isInstanceOf[Structure[_]] && class1 == scrutinee2.getClass && class1 == scrutinee3.getClass
    }
  }


  object Dependent {
    def unapply(scrutinee : (Variational, Variational)) : Option[Int] = {
      val nextCondition1 = scrutinee._1.getCondition
      val nextCondition2 = scrutinee._2.getCondition

      nextCondition1 match {
        case None => nextCondition2
        case Some(condition1) => nextCondition2 match {
          case None => Some(condition1)
          case Some(condition2) => Some(scala.math.min(condition1, condition2))
        }
      }
    }
  }

  /*
  def index[A](values : Seq[VariationalLike[A]], indices : VariationalLike[Int]) : VariationalLike[A] =
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
   */
}

abstract class VFunction1 {
  def apply[A <: VariationalLike[A]](variational : A) : A
}

abstract class VFunction2 {
  def apply[A <: VariationalLike[A]](variational1 : A, variational2 : A) : A
}

class SmartChoice(variable : Int) extends VFunction2 {
  def apply[V <: VariationalLike[V]](variational1 : V, variational2 : V) =
    (variational1, variational2) match {
      case VariationalLike.Equal() =>
        variational1
      case VariationalLike.Constant() =>
        variational1.makeChoice(variable, variational2)
      case VariationalLike.Structural() =>
        variational1.all2(this, variational2).get
      case VariationalLike.Dependent(condition) if variable < condition =>
        variational1.makeChoice(variable, variational2)
      case VariationalLike.Dependent(condition) if variable == condition =>
        variational1.select(condition).makeChoice(variable, variational2.deselect(condition))
      case VariationalLike.Dependent(condition) if variable > condition => {
        val thenBranch : V = apply(variational1.select(condition), variational2.select(condition))
        val elseBranch : V = apply(variational1.deselect(condition), variational2.deselect(condition))
        thenBranch.makeChoice(condition, elseBranch)
      }
    }
}


object Select {
  def apply(variable : Int) = new VFunction1 {
    def apply[A <: VariationalLike[A]](variational : A) : A =
      variational.select(variable)
  }
}

object Deselect {
  def apply(variable : Int) = new VFunction1 {
    def apply[A <: VariationalLike[A]](variational : A) : A =
      variational.deselect(variable)
  }
}

object Choice {
  def apply[A <: VariationalLike[A]](condition : Int, thenBranch : A, elseBranch : A) : A =
    new SmartChoice(condition)(thenBranch, elseBranch)

  def unapply[A <: VariationalLike[A]](variational : Choice[A]) : Some[(Int, A, A)] =
    Some((variational.condition, variational.thenBranch, variational.elseBranch))
}

/*
class Constant[A](val value : A) extends VariationalLike[A] {

  import scala.collection.mutable

  def map[B](f : A => B, cache : mutable.Map[A, VariationalLike[B]]) =
    cache.getOrElseUpdate(value, Constant(f(value)))

  def select(variable : Int) : VariationalLike[A] =
    this

  def deselect(variable : Int) : VariationalLike[A] =
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
 */


/**
 * Structural nodes which may contain further variability in
 * subtrees.
 */
trait Structure[This <: VariationalLike[This]] extends VariationalLike[This] {
  this : This =>
  def exposeChoice = for {
    condition <- getCondition
  } yield {
    (condition, select(condition), deselect(condition))
  }

  def getStructure = Some(getClass)

  def select(variable : Int) = all(Select(variable))

  def deselect(variable : Int) = all(Deselect(variable))
}

abstract class Simple[A, This <: VariationalLike[This]](val value : A) extends Structure[This] {
  this : This =>

  def getCondition = None

  def all(f : VFunction1) = this

  def all2(f : VFunction2, that : This) =
    if (this == that)
      Some(this)
    else
      None

  override def toString =
    value.toString
}

trait SimpleContainer[A, VA <: VariationalContainer[A, VA]] extends VariationalContainer[A, VA] {
  this : Simple[A, VA] with VA =>

  def map[B, VB <: VariationalLike[VB]](f : (A) => B)(implicit lift : Lift[B, VB]) =
    lift(f(value))

  def getValue = Some(value)
}

trait Generic[A] extends VariationalContainer[A, Generic[A]] {
  def newChoice(condition : Int, that : Generic[A]) =
    new Choice(condition, this, that) with ChoiceContainer[A, Generic[A]] with Generic[A]
}

object Generic {
  type Cache[A] = WeakHashMap[A, Generic[A]]
  private[this] val cache : Cache[Any] = WeakHashMap()

  implicit def liftGenericVariational[A] : Lift[A, Generic[A]] = new Lift[A, Generic[A]] {
    def apply(value : A) =
      cache.asInstanceOf[Cache[A]].getOrElseUpdate(value, new Simple[A, Generic[A]](value) with SimpleContainer[A, Generic[A]] with Generic[A])
  }
}

