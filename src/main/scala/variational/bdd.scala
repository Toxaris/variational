package variational

import collection.mutable
import util.parsing.combinator.RegexParsers
import java.net.URL
import io.{BufferedSource, Source}
import variational.BDD.Parser

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
  def fromURL(url : URL) =
    BDD.fromSource(Source.fromURL(url))

  /**
   * Read a propositional expression from a source.
   */
  def fromSource(source : BufferedSource) = {
    val parser = new Parser
    val reader = source.reader();

    val result = parser.parseAll(parser.expression, reader)
    result match {
      case parser.Success(finalResult, _) => {
        val variables = new Array[String](parser.nextVariable)
        for ((key, value) <- parser.variables) {
          variables(value) = key
        }
        (variables, finalResult)
      }
    }
  }

  class UndertakerParser extends Parser {
    def line : Parser[BDD] =
      symbolExpression ~ (("\"" ~> expression <~ "\"" | success(BDD(false)))) ^^ {
        case sym ~ exp => sym -> exp
      }

    def parseLine(text : String) : BDD =
      parseAll(line, text) match {
        case Success(result, _) => result
      }
  }

  def parseUndertaker(source : Source) = {
    val parser = new UndertakerParser

    var result = BDD(true)
    // var i = 0

    for (line <- source.getLines) {
      if (!line.startsWith("I: ")  && !line.startsWith("UNDERTAKER_SET")) {
        // i += 1
        // System.err.println(i + ": " + line.take(60))

        val clause = parser.parseLine(line)

        // val variables = new Array[String](parser.nextVariable)
        // for ((key, value) <- parser.variables) {
        //  variables(value) = key
        // }

        // GraphViz.asFile(result, "x86/result-" + i + ".dot", variables)
        // GraphViz.asFile(clause, "x86/clause-" + i + ".dot", variables)
        result &&= clause
      }
    }

    val variables = new Array[String](parser.nextVariable)
    for ((key, value) <- parser.variables) {
      variables(value) = key
    }
    (variables, result)
  }


  class Parser extends RegexParsers {
    var nextVariable = 0
    val variables : mutable.Map[String, Int] = mutable.Map()

    def stringToVariable(name : String) : BDD =
      BDD(variables.getOrElseUpdate(name, {
        val result = nextVariable
        nextVariable += 1
        result
      }))

    def parseString(text : String) : (Map[String, Int], BDD) =
      parseAll(expression, text) match {
        case Success(result, _) => (variables.toMap, result)
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

  def parseDIMACS(source : Source) : BDD = {
    val lines = source.getLines()

    val tokens = for {
      line <- lines
      if !(line startsWith "c")
      token <- line split """\s+"""
      if token != ""
    } yield (token)

    var result = BDD.High
    var clause = BDD.Low

    val p = tokens.next()
    if (p != "p")
      throw new Error("Missing problem line.")

    val format = tokens.next()
    if (format != "cnf")
      throw new Error("Unknown format '" + format + "'")

    val variables = Integer.parseInt(tokens.next())
    val clauses = Integer.parseInt(tokens.next())

    for (i <- 1 to clauses) {
      var clause = BDD.Low
      var number = Integer.parseInt(tokens.next())

      while (number != 0) {
        if (number < 0)
          clause ||= !BDD(-number - 1)
        else
          clause ||= BDD(number - 1)

        number = Integer.parseInt(tokens.next())
      }

      // println(("// " + i + ": " + clause + ".").take(80))

      result &&= clause
    }

    result
  }

  def parseDIMACS(url : URL) : BDD =
    parseDIMACS(Source.fromURL(url))
}

object VisualizeBDD {
  def main(args : Array[String]) {
    val (variables, bdd) =
      (if (args.length > 0) args(0) else "--formula") match {
        case "--formula" =>
          BDD.fromSource(Source.stdin)
        case "--dimacs" =>
          (Array.empty[String], BDD.parseDIMACS(Source.stdin))
        case "--undertaker" =>
          BDD.parseUndertaker(Source.stdin)
      }

    println(GraphViz.asString(bdd, variables))
  }
}
