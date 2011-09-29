package variational

trait BDD extends VariationalContainer[Boolean, BDD] {
  import BDD.liftBDD

  def newChoice(condition : Int, that : BDD) =
    new Choice(condition, this, that) with ChoiceContainer[Boolean, BDD] with BDD

  def &&(that : BDD) : BDD =
    zipWith(BDD.conjunction, that)

  def ||(that : BDD) : BDD =
    zipWith(BDD.disjunction, that)

  def ->(that : BDD) : BDD =
    zipWith(BDD.implication, that)

  def <=>(that : BDD) : BDD =
    zipWith(BDD.biconditional, that)

  def unary_! : BDD =
    map(BDD.negation)
}

object BDD {
  implicit val liftBDD :Lift[Boolean, BDD] = new Lift[Boolean, BDD] {
    def apply(value : Boolean) = BDD(value)
  }

  type Connective = (Boolean, Boolean) => Boolean

  val conjunction : Connective = (p, q) => p && q
  val disjunction : Connective = (p, q) => p || q
  val implication : Connective = (p, q) => !p || q
  val biconditional : Connective = (p, q) => p == q
  val negation : Boolean => Boolean = p => !p

  def apply(variable : Int) : BDD =
    Choice(variable, High, Low)

  def apply(value : Boolean) : BDD =
    if (value)
      High
    else
      Low

  val High : BDD =
    new Simple[Boolean, BDD](true) with SimpleContainer[Boolean, BDD] with BDD

  val Low : BDD =
    new Simple[Boolean, BDD](false) with SimpleContainer[Boolean, BDD] with BDD
}