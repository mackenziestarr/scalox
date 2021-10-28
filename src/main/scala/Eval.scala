package io.lox
import io.lox.Expression
import io.lox.ExprValue
import io.lox.Token
import io.lox.Expression.*

import scala.util.Try

def eval(expr: Expression): Either[Vector[String], ExprValue] =
  Try(_eval(expr)).fold({
    e => Left(Vector(e.getMessage))
  }, {
    case (x : ExprValue) => Right(x)
  })

def _eval(expr: Expression): ExprValue = expr match
  case Literal(value) => value
  case Grouping(expr) => _eval(expr)
  case Unary(op, right) =>
    val e = _eval(right)
    op match {
      case op : Token.Minus => -e.asInstanceOf[Double]
      case _ => ???
    }


