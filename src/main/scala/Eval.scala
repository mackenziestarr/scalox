import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try


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

def eval(statements: Seq[Statement], env: Environment)(using Console): Either[RuntimeError, Environment] =
  Try {
    statements.foldLeft(env) {
      (env, statement) => Eval.eval(statement).runS(env)
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

  def eval[A](statement: Statement)(using Console): State[Environment, Unit] =
    import Statement.*
    State {
      env => statement match {
        case If(expression, thenBranch, elseBranch) =>
          eval(expression).flatMap { value =>
            if isTruthy(value) then eval(thenBranch)
            else elseBranch.map(eval(_)).getOrElse(State.unit(()))
          }.run(env)
        case Block(statements) =>
          statements.foldLeft(new Environment(Some(env))) {
            (env, statement) =>
              eval(statement).runS(env)
          }
          ((), env)
        case Print(expr) =>
          eval(expr).map {
            e => summon[Console].println(ExprResult.from(e).show)
          }.run(env)
        case Expr(expr) => eval(expr).discard.run(env)
        case Var(name, initializer) =>
          env.define(name.lexeme, initializer.map { expr =>
            eval(expr).runA(env)
          })
          ((), env)
    }
  }

  import Expression.*
  import TokenType.{String as _, Number as _, *}
  def eval(expr: Expression): State[Environment, ExprValue] =
    State {
      env => expr match
        case Assign(name, expr) =>
          eval(expr).map {
            value => env.assign(name, value); value
          }.run(env)
        case Literal(value) => (value, env)
        case Grouping(expr) => eval(expr).run(env)
        case Var(identifier) => (env.get(identifier), env)
        case Unary(op, right) =>
          eval(right).map { value =>
            val result: ExprValue = op.`type` match
              case Minus => value match
                case value: Double => -value.asInstanceOf[Double]
                case _ => throw new RuntimeError(op, s"Operand must be a number.")
              case Bang => !isTruthy(value)
              case _ => throw new RuntimeError(op, s"Operator not supported: '${op.lexeme}$value'")
            result
          }.run(env)
        case Binary(left, op, right) =>
          eval(left).map2[ExprValue, ExprValue](eval(right)) {
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
          }.run(env)

    }



