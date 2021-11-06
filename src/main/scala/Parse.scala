package io.lox
import io.lox.ReservedWords.*
import io.lox.Token
import io.lox.Token.{EqualEqual, Number, ReservedWord}
import scala.collection.immutable.Nil
import io.lox.ExprValue

import scala.annotation.tailrec
import scala.util.Try

enum Statement:
  case Expression(expression: io.lox.Expression)
  case Print(expression: io.lox.Expression)
  case Var(name: Token.Identifier, initializer: Option[io.lox.Expression])

enum Expression:
  def show: String = this match {
    case Assign(name, expr) => s"(${name.lexeme} = ${expr.show})"
    case Binary(left, operator, right) => s"(${operator.lexeme} ${left.show} ${right.show})"
    case Unary(operator, right) => s"(${operator.lexeme} ${right.show})"
    case Grouping(expr) => s"(group ${expr.show})"
    case l: Literal[?] => ExprResult.from(l.value).show
    case Var(name) => s"`${name.lexeme}`"
  }
  case Assign(name: Token.Identifier, expr: Expression)
  case Binary(left: Expression, operator: Token, right: Expression)
  case Unary(operator: Token, right: Expression)
  case Grouping(expr: Expression)
  case Literal[T <: ExprValue ](value: T)
  case Var(name: Token.Identifier)

case class ParseError(message: String, token: Token, tail: List[Token]) extends Throwable {
  override def getMessage = token match {
    case _ : Token.EOF => s"[line ${token.line}] Error at end of file: ${message}"
    case _ => s"[line ${token.line}] Error at '${token.lexeme}': ${message}"
  }
}

def parse(input: List[Token]): Either[ParseError, List[Statement]] =
  // TODO has to be some stdlib function for this
  @tailrec
  def loop(input: List[Token], out: List[Option[Statement]]): List[Option[Statement]] = {
    input.headOption match
      case Some(Token.EOF(_)) => out.reverse
      case _ =>
        val (remaining, statement) = Productions.declaration(input)
        loop(remaining, statement :: out)
  }
  Try(loop(input, List.empty)).toEither
    .map(_.flatten)
    .left.map {
      case e: ParseError => e
    }

/**
 * TODO docblock
 * @param t
 * @return
 */
private def isSyncToken(t: Token) = t match
  case ReservedWord(_, `class`, _) => true
  case ReservedWord(_, `fun`, _) => true
  case ReservedWord(_, `var`, _) => true
  case ReservedWord(_, `for`, _) => true
  case ReservedWord(_, `if`, _) => true
  case ReservedWord(_, `while`, _) => true
  case ReservedWord(_, `print`, _) => true
  case ReservedWord(_, `return`, _) => true
  case _: Token.EOF => true
  case _ => false

private def synchronize(input: List[Token]) = input.dropWhile(!isSyncToken(_))

final case class NonEmptyList[+A](head: A, tail: List[A])

private object Productions:
  import io.lox.Expression.*
  import io.lox.Token.*
  type Comparison = GreaterThan | GreaterThanEqual | LessThan | LessThanEqual
  type Term = Minus | Plus
  type Factor = Star | Slash
  type Unary = Bang | Minus

  inline def binaryMatch[A <: Token](next: List[Token] => (List[Token], Expression))(input: List[Token]) =
    val (in, left) = next(input)
    @tailrec
    def loop(input: List[Token], expr: Expression): (List[Token], Expression) = {
      // TODO unsafe
      input.head match
        case t : A =>
          val (i, right) = next(input.drop(1))
          loop(i, Binary(expr, t, right))
        case _ => (input, expr)
    }
    loop(in, left)

  def declaration(input: List[Token]): (List[Token], Option[Statement]) =
    try {
      val (out, stmt) = input.head match {
        case ReservedWord(_, `var`, _) => varDeclaration(input.drop(1))
        case _ => statement(input)
      }
      (out, Some(stmt))
    } catch {
      case (e: ParseError) =>
        println(e.getMessage)
        // TODO do better than stuffing this into `e`
        val remaining = synchronize(e.tail)
        (remaining, None)
    }

  def varDeclaration(input: List[Token]) =
    // TODO unsafe
    input match
      case (name @ Identifier(_, _)) :: Semicolon(_) :: rest => (rest, Statement.Var(name, None))
      case (name @ Identifier(_, _)) :: Equal(_) :: rest =>
        val (remaining, initializer) = expression(rest)
        remaining.head match
          case Semicolon(_) => (remaining.tail, Statement.Var(name, Some(initializer)))
          case t => throw ParseError("Expect ';' after variable declaration.", t, remaining)
      case _ => ???

  def statement(input: List[Token]) =
    // TODO unsafe
    input.head match
      case ReservedWord(_, `print`, _) => printStatement(input.drop(1))
      case _ => expressionStatement(input)

  def printStatement(input: List[Token]) =
    val (remaining, expr) = expression(input)
    // TODO unsafe
    remaining.head match
      case _: Semicolon => (remaining.drop(1), Statement.Print(expr))
      case t => throw ParseError("Expect ';' after value.", t, remaining)

  def expressionStatement(input: List[Token]) =
    val (remaining, expr) = expression(input)
    // TODO unsafe
    remaining.head match
      case _: Semicolon => (remaining.drop(1), Statement.Expression(expr))
      case t => throw ParseError("Expect ';' after expression.", t, remaining)

  def expression(input: List[Token]) = assignment(input)

  def assignment(input: List[Token]): (List[Token], Expression) =
    val (out, expr) = equality(input)
    out.head match
      case t: Equal =>
        expr match
          case Var(name) =>
            val (tail, value) = assignment(out.tail)
            (tail, Assign(name, value))
          case _ =>
            // TODO not supposed to throw here, just report
            // https://github.com/munificent/craftinginterpreters/blob/master/java/com/craftinginterpreters/lox/Lox.java#L109-L117
            throw ParseError("Invalid assignment target.", t, out)
      case _ => (out, expr)

  def equality(input: List[Token])   = binaryMatch[EqualEqual](comparison _)(input)
  def comparison(input: List[Token]) = binaryMatch[Comparison](term _)(input)
  def term(input: List[Token])       = binaryMatch[Term](factor _)(input)
  def factor(input: List[Token])     = binaryMatch[Factor](unary _)(input)

  def unary(input: List[Token]): (List[Token], Expression) =
    // TODO unsafe
    input.head match
      case t : Unary =>
        val (in, right) = unary(input.drop(1))
        (in, Unary(t, right))
      case _ => primary(input)

  def primary(input: List[Token]): (List[Token], Expression) =
    // TODO unsafe
    input.head match
      case t : Token.String => (input.drop(1), Literal(t.value))
      case t : Token.Number => (input.drop(1), Literal(t.value))
      case t : Identifier => (input.drop(1), Var(t))
      case ReservedWord(lexeme, `true`, _) => (input.drop(1), Literal(true))
      case ReservedWord(lexeme, `false`, _) => (input.drop(1), Literal(false))
      case ReservedWord(lexeme, `nil`, _) => (input.drop(1), Literal(null))
      case t : LeftParenthesis =>
        val (i, expr) = expression(input.drop(1))
        i.head match
          case t : RightParenthesis => (i.drop(1), Grouping(expr))
          case t => throw ParseError("Expected ')' after expression", t, input)
      case t => throw ParseError("Expected expression", t, input)