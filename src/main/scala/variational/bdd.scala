package variational

import collection.mutable
import util.parsing.combinator.RegexParsers
import io.Source
import java.net.URL

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
  implicit val liftBDD : Lift[Boolean, BDD] = new Lift[Boolean, BDD] {
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

  /**
   * Read a propositional expression from a string.
   *
   * <p>This operation uses an permissible grammar for
   * propositional expressions, allowing the following syntax:
   *
   * <ul>
   *   <li>Variables:
   *     <code>def(</code><it>identifier</it><code>)</code>,
   *     <code>macro(</code><it>identifier</it><code>)</code>,
   *     <it>identifier</it>
   *   <li>Negation:
   *     <code>!</code>,
   *     <code>~</code>,
   *     <code>not</code>
   *   <li>Conjunction:
   *     <code>&</code>,
   *     <code>&&</code>,
   *     <code>/\</code>,
   *     <code>and</code>
   *   <li>Disjunction:
   *     <code>|</code>,
   *     <code>||</code>,
   *     <code>\/</code>,
   *     <code>or</code>
   *   <li>Implication:
   *     <code>-></code>,
   *     <code>=></code>,
   *     <code>implies</code>
   *   <li>Biconditional:
   *     <code>=</code>,
   *     <code>==</code>,
   *     <code><-></code>,
   *     <code><=></code>,
   *     <code>iff</code>,
   *     <code>if and only if</code>
   * </ul>
   *
   * <p>This function assigns numbers to variable names in order
   * of their first appearance in the string.
   */
  def fromString(text : String) =
    (new Parser).parseString(text)

  /**
   * Read a propositional expression from a url.
   */
  def fromURL(url : URL) = {
    val parser = new Parser
    val reader = Source.fromURL(url).reader()

    val result = parser.parseAll(parser.expression, reader)
    result match {
      case parser.Success(result, _) => result
    }
  }

  class Parser extends RegexParsers {
    var nextVariable = 0
    val variables : mutable.Map[String, BDD] = mutable.Map()

    def stringToVariable(name : String) : BDD =
      variables.getOrElseUpdate(name, {
        val result = BDD(nextVariable)
        nextVariable += 1
        result
      })

    def parseString(text : String) : BDD =
      parseAll(expression, text) match {
        case Success(result, _) => result
      }

    def expression =
      biconditionalExpression

    def identifier : Parser[String]
    = "[a-zA-Z_][a-zA-Z_0-9]*".r

    def symbolExpression : Parser[BDD] =
      ("def" ~> "(" ~> identifier <~ ")"
        | "macro" ~> "(" ~> identifier <~ ")"
        | identifier) ^^ stringToVariable

    def atomicExpression : Parser[BDD] =
      (symbolExpression | "(" ~> expression <~ ")")

    def literalExpression : Parser[BDD] =
      unarySymbol ~ atomicExpression ^^ {
        case (operator ~ operand) => operator(operand)
      } | atomicExpression

    def conjunctiveExpression : Parser[BDD] =
      chainl1(literalExpression, conjunctionSymbol)

    def disjunctiveExpression : Parser[BDD] =
      chainl1(conjunctiveExpression, disjunctionSymbol)

    def implicativeExpression : Parser[BDD] =
      chainl1(disjunctiveExpression, implicationSymbol)

    def biconditionalExpression : Parser[BDD] =
      chainl1(implicativeExpression, biconditionalSymbol)

    def conjunctionSymbol : Parser[(BDD, BDD) => BDD] =
      ("&&" | "&" | """/\""" | "and") ^^ {
        case _ => _ && _
      }

    def disjunctionSymbol : Parser[(BDD, BDD) => BDD] =
      ("||" | "|" | """\/""" | "or") ^^ {
        case _ => _ || _
      }

    def implicationSymbol : Parser[(BDD, BDD) => BDD] =
      ("->" | "=>" | "implies") ^^ {
        case _ => _ -> _
      }

    def biconditionalSymbol : Parser[(BDD, BDD) => BDD] =
      ("==" | "=" | "<=>" | "<->" | "iff" | "if" ~> "and" ~> "only" ~> "if") ^^ {
        case _ => _ <=> _
      }


    def unarySymbol : Parser[BDD => BDD] =
      ("!" | "~" | "not") ^^ {
        case _ => !_
      }

  }

}
