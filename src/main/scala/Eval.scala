package io.lox

import io.lox.Expression
import io.lox.ExprValue
import io.lox.Token
import io.lox.Expression.*
import io.lox.Statement

import scala.collection.mutable
import scala.util.Try

class RuntimeError(val token: Token, message: String) extends RuntimeException(message):
  def show = s"${message}\n[line ${token.line}]"

object Environment:
  private val values = mutable.Map.empty[String, ExprValue]
  def define(name: String, value: Option[ExprValue]) = values.update(name, value.getOrElse(null))
  def get(name: Token.Identifier) = values.getOrElse(name.lexeme,
    throw new RuntimeError(name, s"Undefined variable `${name.lexeme}`.")
  )

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
    case s: String => s"\"$s\""
    case _ => value.toString
extension (e: ExprResult)
  def show: String = ExprResult.show(e)

sealed trait Printer:
  def show(e: ExprValue): Unit

object ConsolePrinter extends Printer:
  override def show(e: ExprValue): Unit =
    println(ExprResult.from(e).show)

class InMemoryPrinter extends Printer:
  private var list = List.empty[String]
  def get = list.reverse
  override def show(e: ExprValue): Unit =
    list = ExprResult.from(e).show :: list

// TODO typeclass pattern with printers
def eval(statements: Seq[Statement], p: Printer): Either[RuntimeError, Unit] =
  Try {
    for (statement <- statements)
      do Eval.eval(statement, p)
  }.toEither.left.map {
    case e: RuntimeError => e
  }

private[this] object Eval:
  import io.lox.Token.{String as _, *}
  def isTruthy(value: ExprValue): Boolean = value match
    case _: Null => false
    case b: Boolean => b
    case _ => true

  def eval[A](statement: Statement, p: Printer): Unit = statement match {
    case Statement.Print(expr) => p.show(eval(expr))
    case Statement.Expression(expr) => eval(expr)
    case Statement.Var(name, initializer) =>
      Environment.define(name.lexeme, initializer.map(eval(_)))
  }
  def eval(expr: Expression): ExprValue = expr match
    case Literal(value) => value
    case Grouping(expr) => eval(expr)
    case Var(identifier) => Environment.get(identifier)
    case Unary(op, right) =>
      val value = eval(right)
      op match
        case _: Minus => value match
          case value: Double => -value.asInstanceOf[Double]
          // TODO would like to use value.show here from ExprResult
          case _ => throw new RuntimeError(op, s"Operand must be a number in '${op.lexeme}$value'")
        case _: Bang => !isTruthy(value)
        case _ => throw new RuntimeError(op, s"Operator not supported: '${op.lexeme}$value'")
    case Binary(left, op, right) =>
      (eval(left), eval(right)) match
        case (left: Double, right: Double) => op match {
          case _: Minus => left - right
          case _: Slash => left / right
          case _: Star => left * right
          case _: Plus => left + right
          case _: GreaterThan => left > right
          case _: GreaterThanEqual => left >= right
          case _: LessThan => left < right
          case _: LessThanEqual => left <= right
          case _: EqualEqual => left == right
          case _: BangEqual => left != right
          case _ => throw new RuntimeError(op, s"Operator not supported: '$left ${op.lexeme} $right'")
        }
        case (left: String, right: String) => op match {
          case _: Plus => left + right
          case _: EqualEqual => left == right
          case _: BangEqual => left != right
          case _ => throw new RuntimeError(op, s"Operator not supported: '$left ${op.lexeme} $right'")
        }
        case (left, right) => op match {
          case _: EqualEqual => left == right
          case _: BangEqual => left != right
          case _ => throw new RuntimeError(op, s"Operator not supported: '$left ${op.lexeme} $right'")
        }



