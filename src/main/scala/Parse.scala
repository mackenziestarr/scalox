package io.lox
import io.lox.ReservedWords.*
import io.lox.Token
import io.lox.Token.{EqualEqual, Number, ReservedWord}
import scala.collection.immutable.Nil

import scala.annotation.tailrec
import scala.util.Try

type ExprValue = String | Double | Boolean | Null

// TODO maybe i went too hard here
opaque type ExprResult = ExprValue
object ExprResult:
  def from(value: ExprValue): ExprResult = value
  def show(value: ExprResult): String = value match
    case _ : Null => "nil"
    case d : Double =>
      val s : String = d.toString
      val (left, right) = s.span(_ != '.')
      if right == ".0" then left else s
    case _ => value.toString
extension (e: ExprResult)
  def show: String = ExprResult.show(e)

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

def parse(input: Seq[Token]): Either[Vector[String], Expression] =
  Try(Productions.expression(input)).fold({
    (e: Throwable) => Left(Vector(e.getMessage))
  }, {
    (remaining, expr) =>
      if remaining.tail.nonEmpty
      then Left(Vector(s"error: ${remaining.tail}"))
      else Right(expr)
  })

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
      case t @ ReservedWord(lexeme, `true`, _) => (input.drop(1), Literal(true))
      case t @ ReservedWord(lexeme, `false`, _) => (input.drop(1), Literal(false))
      case t @ ReservedWord(lexeme, `nil`, _) => (input.drop(1), Literal(null))
      case t : LeftParenthesis =>
        val (i, expr) = expression(input.drop(1))
        i.head match
          case t : RightParenthesis => (i.drop(1), Grouping(expr))
          case t => throw ParseError("Expected ')' after expression", t)
      case t => throw ParseError("Expected expression", t)