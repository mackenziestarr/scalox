import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try
import cats.data.State as CatsState
import cats.syntax.apply._

class Environment(parent: Option[Environment]):
  private val values = mutable.Map.empty[String, ExprValue]
  def define(name: String, value: Option[ExprValue]) =
    values.update(name, value.getOrElse(null))
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

type ExprValue = String | Double | Boolean | Null
def display(value: ExprValue) = value match
  case null => "nil"
  case num : Double =>
    val original = num.toString
    val (left, right) = original.span(_ != '.')
    if right == ".0" then left else original
  case _ => value.toString

def eval(statements: Seq[Statement], env: Environment): Either[RuntimeError, Environment] =
  Try {
    statements.foldLeft(env) {
      (env, statement) => Eval.eval(statement).runS(env).value
    }
  }.toEither.left.map {
    case e: RuntimeError => e
  }

private[this] object Eval:
  import TokenType.{String as _, *}
  def isTruthy(value: ExprValue): Boolean = value match
    case null => false
    case b: Boolean => b
    case _ => true

  def eval[A](statement: Statement): CatsState[Environment, Unit] =
    import Statement.*
    CatsState {
      env => statement match {
        case If(expression, thenBranch, elseBranch) =>
          eval(expression).flatMap { value =>
            if isTruthy(value) then eval(thenBranch)
            else elseBranch.map(eval(_)).getOrElse(CatsState.pure(()))
          }.run(env).value
        case While(condition, body) =>
          while (isTruthy(eval(condition).runA(env).value))
            eval(body).run(env).value
          (env, ())
        case Block(statements) =>
          statements.foldLeft(new Environment(Some(env))) {
            (env, statement) =>
              eval(statement).runS(env).value
          }
          (env, ())
        case Print(expr) =>
          eval(expr).map {
            e => println(display(e))
          }.run(env).value
        case Expr(expr) => eval(expr).map(_ => ()).run(env).value
        case Var(name, initializer) =>
          env.define(name.lexeme, initializer.map { expr =>
            eval(expr).runA(env).value
          })
          (env, ())
    }
  }

  import Expression.*
  import TokenType.{String as _, Number as _, *}
  def eval(expr: Expression): CatsState[Environment, ExprValue] =
    CatsState {
      env => expr match
        case Assign(name, expr) =>
          eval(expr).map {
            value => env.assign(name, value); value
          }.run(env).value
        case Literal(value) => (env, value)
        case Grouping(expr) => eval(expr).run(env).value
        case Var(identifier) => (env, env.get(identifier))
        case Unary(op, right) =>
          eval(right).map { value =>
            val result: ExprValue = op.`type` match
              case Minus => value match
                case value: Double => -value.asInstanceOf[Double]
                case _ => throw new RuntimeError(op, s"Operand must be a number.")
              case Bang => !isTruthy(value)
              case _ => throw new RuntimeError(op, s"Operator not supported: '${op.lexeme}$value'")
            result
          }.run(env).value
        case Logical(left, op, right) =>
          import ReservedWords.{`and`, `or`}
          val state = op.`type` match {
            case ReservedWord(`or`) => eval(left).flatMap { value =>
              if isTruthy(value) then CatsState.pure(value) else eval(right)
            }
            case ReservedWord(`and`) => eval(left).flatMap { value =>
              if isTruthy(value) then eval(right) else CatsState.pure(value)
            }
            case _ => throw new RuntimeError(op, s"Operator not supported: '${op.lexeme}'")
          }
          state.run(env).value
        case Binary(left, op, right) =>
          (eval(left), eval(right)).mapN[ExprValue] {
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
            }
          }.run(env).value
    }



