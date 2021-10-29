package io.lox
import io.lox.ReservedWords.*
import io.lox.Token
import io.lox.Token.{EqualEqual, Number, ReservedWord}
import scala.collection.immutable.Nil
import io.lox.ExprValue

import scala.annotation.tailrec
import scala.util.Try

enum Statement(val expression: Expression):
  case Expression(override val expression: io.lox.Expression) extends Statement(expression)
  case Print(override val expression: io.lox.Expression) extends Statement(expression)

enum Expression:
  def show: String = this match {
    case Binary(left, operator, right) => s"(${operator.lexeme} ${left.show} ${right.show})"
    case Unary(operator, right) => s"(${operator.lexeme} ${right.show})"
    case Grouping(expr) => s"(group ${expr.show})"
    case l: Literal[?] => ExprResult.from(l.value).show
  }
  case Binary(left: Expression, operator: Token, right: Expression)
  case Unary(operator: Token, right: Expression)
  case Grouping(expr: Expression)
  case Literal[T <: ExprValue ](value: T)

case class ParseError(message: String, token: Token) extends Throwable {
  override def getMessage = token match {
    case _ : Token.EOF => s"[line ${token.line}] Error at end of file: ${message}"
    case _ => s"[line ${token.line}] Error at '${token.lexeme}': ${message}"
  }
}

def parse(input: Seq[Token]): Either[ParseError, Vector[Statement]] =
  // TODO has to be some stdlib function for this
  @tailrec
  def loop(input: Seq[Token], out: Vector[Statement]): Vector[Statement] = {
    input.headOption match
      case Some(Token.EOF(_)) => out
      case _ =>
        val (remaining, statement) = Productions.statement(input)
        loop(remaining, out :+ statement)
  }
  Try(loop(input, Vector.empty)).toEither.left.map {
    case e: ParseError => e
  }

private def isSyncToken(t: Token) = t match
  case _: Token.Semicolon => true
  case ReservedWord(_, `class`, _) => true
  case ReservedWord(_, `fun`, _) => true
  case ReservedWord(_, `var`, _) => true
  case ReservedWord(_, `for`, _) => true
  case ReservedWord(_, `if`, _) => true
  case ReservedWord(_, `while`, _) => true
  case ReservedWord(_, `print`, _) => true
  case ReservedWord(_, `return`, _) => true
  case _ => false
private def synchronize(input: Seq[Token]) = input.dropWhile(!isSyncToken(_))

private object Productions:
  import io.lox.Expression.*
  import io.lox.Token.*
  type Comparison = GreaterThan | GreaterThanEqual | LessThan | LessThanEqual
  type Term = Minus | Plus
  type Factor = Star | Slash
  type Unary = Bang | Minus

  inline def binaryMatch[A <: Token](next: Seq[Token] => (Seq[Token], Expression))(input: Seq[Token]) =
    val (in, left) = next(input)
    @tailrec
    def loop(input: Seq[Token], expr: Expression): (Seq[Token], Expression) = {
      // TODO unsafe
      input.head match
        case t : A =>
          val (i, right) = next(input.drop(1))
          loop(i, Binary(expr, t, right))
        case _ => (input, expr)
    }
    loop(in, left)

  def statement(input: Seq[Token]) =
    // TODO unsafe
    input.head match
      case ReservedWord(_, `print`, _) => printStatement(input.drop(1))
      case _ => expressionStatement(input)

  def printStatement(input: Seq[Token]) =
    val (remaining, expr) = expression(input)
    // TODO unsafe
    remaining.head match
      case _: Semicolon => (remaining.drop(1), Statement.Print(expr))
      case t => throw ParseError("Expect ';' after value.", t)

  def expressionStatement(input: Seq[Token]) =
    val (remaining, expr) = expression(input)
    // TODO unsafe
    remaining.head match
      case _: Semicolon => (remaining.drop(1), Statement.Expression(expr))
      case t => throw ParseError("Expect ';' after value.", t)

  def expression(input: Seq[Token]) = equality(input)
  def equality(input: Seq[Token])   = binaryMatch[EqualEqual](comparison _)(input)
  def comparison(input: Seq[Token]) = binaryMatch[Comparison](term _)(input)
  def term(input: Seq[Token])       = binaryMatch[Term](factor _)(input)
  def factor(input: Seq[Token])     = binaryMatch[Factor](unary _)(input)

  def unary(input: Seq[Token]): (Seq[Token], Expression) =
    // TODO unsafe
    input.head match
      case t : Unary =>
        val (in, right) = unary(input.drop(1))
        (in, Unary(t, right))
      case _ => primary(input)

  def primary(input: Seq[Token]): (Seq[Token], Expression) =
    // TODO unsafe
    input.head match
      case t : Token.String => (input.drop(1), Literal(t.value))
      case t : Token.Number => (input.drop(1), Literal(t.value))
      case ReservedWord(lexeme, `true`, _) => (input.drop(1), Literal(true))
      case ReservedWord(lexeme, `false`, _) => (input.drop(1), Literal(false))
      case ReservedWord(lexeme, `nil`, _) => (input.drop(1), Literal(null))
      case t : LeftParenthesis =>
        val (i, expr) = expression(input.drop(1))
        i.head match
          case t : RightParenthesis => (i.drop(1), Grouping(expr))
          case t => throw ParseError("Expected ')' after expression", t)
      case t => throw ParseError("Expected expression", t)