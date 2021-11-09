import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try


class Environment(parent: Option[Environment]):
  private val values = mutable.Map.empty[String, ExprValue]
  def define(name: String, value: Option[ExprValue]) = values.update(name, value.getOrElse(null))
  def assign(name: Token, value: ExprValue): Unit = {
    if values.contains(name.lexeme) then define(name.lexeme, Some(value))
    else parent match
      case Some(env) => env.assign(name, value)
      case None => throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
  }
  def get(name: Token): ExprValue =
    if values.contains(name.lexeme) then
      values.getOrElse(name.lexeme,
        throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'."))
    else
      parent match {
      case Some(env) => env.get(name)
      case None => throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
    }

object Environment:
  private val values = mutable.Map.empty[String, ExprValue]
  def assign(name: Token, value: ExprValue) = {
    get(name)
    define(name.lexeme, Some(value))
  }
  def define(name: String, value: Option[ExprValue]) = values.update(name, value.getOrElse(null))
  def get(name: Token) = values.getOrElse(name.lexeme,
    throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
  )

type ExprValue = String | Double | Boolean | Null

// TODO maybe i went too hard here
opaque type ExprResult = ExprValue
object ExprResult:
  def from(value: ExprValue): ExprResult = value
  def show(value: ExprResult): String = value match
    case null => "nil"
    case d : Double =>
      val s : String = d.toString
      val (left, right) = s.span(_ != '.')
      if right == ".0" then left else s
    case s: String => s
    case _ => value.toString
extension (e: ExprResult)
  def show: String = ExprResult.show(e)

trait Console:
  def println(s: String): Unit

def eval(statements: Seq[Statement])(using Console): Either[RuntimeError, Unit] =
  Try {
    for (statement <- statements)
      do Eval.eval(statement)
  }.toEither.left.map {
    case e: RuntimeError => e
  }

private[this] object Eval:
  import TokenType.{String as _, *}
  def isTruthy(value: ExprValue): Boolean = value match
    case null => false
    case b: Boolean => b
    case _ => true

  def eval[A](statement: Statement)(using Console): Unit = statement match {
    case Statement.Print(expr) => summon[Console].println(ExprResult.from(eval(expr)).show)
    case Statement.Expr(expr) => eval(expr)
    case Statement.Var(name, initializer) =>
      Environment.define(name.lexeme, initializer.map(eval(_)))
  }

  import Expression.*
  import TokenType.{String as _, Number as _, *}
  def eval(expr: Expression): ExprValue = expr match
    case Assign(name, expr) =>
      val value = eval(expr)
      Environment.assign(name, value)
      value
    case Literal(value) => value
    case Grouping(expr) => eval(expr)
    case Var(identifier) =>Environment.get(identifier)
    case Unary(op, right) =>
      val value = eval(right)
      op.`type` match
        case Minus => value match
          case value: Double => -value.asInstanceOf[Double]
          // TODO would like to use value.show here from ExprResult
          case _ => throw new RuntimeError(op, s"Operand must be a number.")
        case Bang => !isTruthy(value)
        case _ => throw new RuntimeError(op, s"Operator not supported: '${op.lexeme}$value'")
    case Binary(left, op, right) =>
      (eval(left), eval(right)) match
        case (left: Double, right: Double) => op.`type` match {
          case Minus => left - right
          case Slash => left / right
          case Star => left * right
          case Plus => left + right
          case GreaterThan => left > right
          case GreaterThanEqual => left >= right
          case LessThan => left < right
          case LessThanEqual => left <= right
          case EqualEqual => left == right
          case BangEqual => left != right
          case _ => throw new RuntimeError(op, s"Operator not supported: '$left ${op.lexeme} $right'")
        }
        case (left: String, right: String) => op.`type` match {
          case Plus => left + right
          case EqualEqual => left == right
          case BangEqual => left != right
          case _ => throw new RuntimeError(op, s"Operator not supported: '$left ${op.lexeme} $right'")
        }
        case (left, right) => op.`type` match {
          case EqualEqual => left == right
          case BangEqual => left != right
          case Plus => throw new RuntimeError(op, s"Operands must be two numbers or two strings.")
          case _ => throw new RuntimeError(op, s"Operands must be numbers.")
          //case _ => throw new RuntimeError(op, s"Operator not supported: '$left ${op.lexeme} $right'")
        }



