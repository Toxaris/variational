package variational.stlc

import variational._

/**
 * Variational simple types.
 *
 * @author Tillmann Rendel
 */

trait Type extends VariationalContainer[Type.Structure, Type] {
  def newChoice(condition : Int, that : Type) =
    new Choice(condition, this, that) with ChoiceContainer[Type.Structure, Type] with Type
}

object Type {
  implicit def liftType : Lift[Structure, Type] = new Lift[Structure, Type] {
    def apply(value : Structure) =
      value
  }

  trait Structure extends Type with StructureLike[Type] with SelfContainer[Structure, Type]

  class Var(val identifier : String) extends Structure with Leaf[Type] {
    def prefix = "Var"
  }

  class Fun(val domain : Type, val codomain : Type) extends Structure {
    def all(f : VFunction1) =
      Fun(f(domain), f(codomain))

    def all2(f : VFunction2, that : Type) =
      if (that.isInstanceOf[Fun]) {
        val other = that.asInstanceOf[Fun]
        Some(Fun(f(domain, other.domain), f(codomain, other.codomain)))
      } else {
        None
      }

    def children = Seq(domain, codomain)

    def prefix = "Fun"
  }

  object Fun {
    def apply(domain : Type, codomain : Type) =
      new Fun(domain, codomain)
  }

}