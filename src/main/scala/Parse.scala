package io.lox
import io.lox.ReservedWords.{`false`, `nil`, `true`}
import io.lox.Token
import io.lox.Token.{EqualEqual, Number}

import scala.annotation.tailrec

enum Expression:
  def show: String = this match {
    case Binary(left, operator, right) => s"(${operator.lexeme} ${left.show} ${right.show})"
    case Unary(operator, right) => s"(${operator.lexeme} ${right.show})"
    case Grouping(expr) => s"(group ${expr.show})"
    case Literal(token) => token match
      case s: Token.String => s.value
      case n: Token.Number => n.value.toString
      case Token.ReservedWord(lexeme, `true`, _) => lexeme
      case Token.ReservedWord(lexeme, `false`, _) => lexeme
      case Token.ReservedWord(lexeme, `nil`, _) => lexeme
      case _ => ""
  }
  case Binary(left: Expression, operator: Token, right: Expression)
  case Unary(operator: Token, right: Expression)
  case Grouping(expr: Expression)
  case Literal(token: Token.String | Token.Number | Token.ReservedWord)

def parse(input: Seq[Token]): Either[Vector[String], Expression] = {
  val (remaining, expr) = Productions.expression(input)
  if remaining.tail.nonEmpty then Left(Vector(s"error: ${remaining.tail}")) else Right(expr)
}

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
      input.head match
        case t : A =>
          val (i, right) = next(input.drop(1))
          loop(i, Binary(expr, t, right))
        case _ => (input, expr)
    }
    loop(in, left)


  def expression(input: Seq[Token]) = equality(input)
  def equality(input: Seq[Token])   = binaryMatch[EqualEqual](comparison _)(input)
  def comparison(input: Seq[Token]) = binaryMatch[Comparison](term _)(input)
  def term(input: Seq[Token])       = binaryMatch[Term](factor _)(input)
  def factor(input: Seq[Token])     = binaryMatch[Factor](unary _)(input)

  def unary(input: Seq[Token]): (Seq[Token], Expression) =
    input.head match
      case t : Unary =>
        val (in, right) = unary(input.drop(1))
        (in, Unary(t, right))
      case _ => primary(input)

  def primary(input: Seq[Token]): (Seq[Token], Expression) =
    input.head match
      case t : (Token.Number | Token.String) => (input.drop(1), Literal(t))
      case t @ Token.ReservedWord(lexeme, `true`, _) => (input.drop(1), Literal(t))
      case t @ Token.ReservedWord(lexeme, `false`, _) => (input.drop(1), Literal(t))
      case t @ Token.ReservedWord(lexeme, `nil`, _) => (input.drop(1), Literal(t))
      case t : LeftParenthesis =>
        val (i, expr) = expression(input.drop(1))
        i.head match
          case t : RightParenthesis => (i.drop(1), Grouping(expr))
          case _ => ??? // TODO(@mstarr) 'Expected ')' after expression'
      case _ => ???