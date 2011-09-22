package variational

object BDD {
  type BDD = V[Boolean]

  def apply(boolean : Boolean) : BDD = V(boolean)
  def apply(variable : Int) : BDD = Choice(variable, BDD(true), BDD(false))

  implicit def pimpBDD(lhs : BDD) = new {
    def &&(rhs : BDD) =
      lhs.zipWith((x : Boolean, y : Boolean) => x && y, rhs)

    def ||(rhs : BDD) =
      lhs.zipWith((x : Boolean, y : Boolean) => x || y, rhs)

    def ->(rhs : BDD) =
      lhs.zipWith((x : Boolean, y : Boolean) => !x || y, rhs)

    def <=>(rhs : BDD) =
      lhs.zipWith((x : Boolean, y : Boolean) => x == y, rhs)

    def unary_!() = lhs.map(!_)
  }
}