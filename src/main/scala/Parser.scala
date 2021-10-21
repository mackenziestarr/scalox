package io.lox
import io.lox.ReservedWords.{`false`, `nil`, `true`}
import io.lox.Token
import io.lox.Token.{EqualEqual, Number}

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

def parse(input: Seq[Token]): Either[String, Expression] = {
  val (remaining, expr) = Productions.expression(input)
  if remaining.tail.nonEmpty then Left("error") else Right(expr)
}


// 2 < 3 == 3 <= 4

private object Productions:
  import io.lox.Expression.*
  import io.lox.Token.*
  def expression(input: Seq[Token]) = equality(input)
  def equality(input: Seq[Token]): (Seq[Token], Expression) =
    val (in, left) = comparison(input)
    in.head match
      case t : EqualEqual =>
        val (i, right) = comparison(in.drop(1))
        (i, Binary(left, t, right))
      case _ => ???
  def comparison(input: Seq[Token]): (Seq[Token], Expression) =
    val (in, left) = term(input)
    in.head match
      case t : (GreaterThan | GreaterThanEqual | LessThan | LessThanEqual) =>
        val (i, right) = term(in.drop(1))
        (i, Binary(left, t, right))
      case _ => (in, left)
  def term(input: Seq[Token]): (Seq[Token], Expression) =
    val (in, left) = factor(input)
    in.head match
      case t : (Minus | Plus) =>
        val (i, right) = factor(in.drop(1))
        (i, Binary(left, t, right))
      case _ => (in, left)
  def factor(input: Seq[Token]): (Seq[Token], Expression) =
    val (in, left) = primary(input)
    in.head match
      case t : (Star | Slash) =>
        val (i, right) = primary(in.drop(1))
        (i, Binary(left, t, right))
      case _ => (in, left)
  def primary(in: Seq[Token]): (Seq[Token], Expression) =
    in.head match
      case t : Number => (in.drop(1), Literal(t))
      case _ => ???