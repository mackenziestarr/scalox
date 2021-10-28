package io.lox
import io.lox.Expression
import io.lox.ExprValue
import io.lox.Token
import io.lox.Expression.*

import scala.util.Try

def eval(expr: Expression): Either[Vector[String], ExprResult] =
  Try(Eval.eval(expr)).fold(e => Left(Vector(e.getMessage)), x => Right(ExprResult.from(x)))

private[this] object Eval:
  def isTruthy(value: ExprValue): Boolean = value match
    case _ : Null => false
    case b : Boolean => b
    case _ => true
  def eval(expr: Expression): ExprValue = expr match
    case Literal(value) => value
    case Grouping(expr) => eval(expr)
    case Unary(op, right) =>
      val value = eval(right)
      op match
        case _: Token.Minus => -value.asInstanceOf[Double]
        case _: Token.Bang => !isTruthy(value)
        case _ => ???
    case Binary(left, op, right) =>
      (eval(left), eval(right)) match
        case (left: Double, right: Double) => op match {
          case _: Token.Minus => left - right
          case _: Token.Slash => left / right
          case _: Token.Star => left * right
          case _: Token.Plus => left + right
          case _ => ???
        }
        case (left: String, right: String) => op match {
          case _: Token.Plus => left + right
          case _ => ???
        }
        case _ => ???



