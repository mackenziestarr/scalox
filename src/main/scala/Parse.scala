import ReservedWords.*

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import cats.data.State as CatsState
import cats.implicits._

// TODO get rid of all List.head usage

enum Statement:
  case Block(statements: List[Statement])
  case If(condition: Expression, `then`: Statement, `else`: Option[Statement])
  case While(condition: Expression, body: Statement)
  case Expr(expression: Expression)
  case Print(expression: Expression)
  case Var(name: Token, initializer: Option[Expression])

enum Expression:
  case Assign(name: Token, expr: Expression)
  case Binary(left: Expression, operator: Token, right: Expression)
  case Logical(left: Expression, operator: Token, right: Expression)
  case Unary(operator: Token, right: Expression)
  case Grouping(expr: Expression)
  case Literal[T <: ExprValue](value: T)
  case Var(name: Token)

def parse(input: List[Token]): Either[ParseErrors, List[Statement]] =
  @tailrec
  def loop(input: List[Token], errors: List[ParseError], statements: List[Statement]): (List[ParseError], List[Statement]) = {
    input.headOption match
      case None => (errors.reverse, statements.reverse)
      case Some(Token(TokenType.EOF)) => (errors.reverse, statements.reverse)
      case _ =>
        Productions.declaration.run(input).value match
          case (remaining, Right(statement)) => loop(remaining, errors, statement :: statements)
          case (remaining, Left(error)) => loop(synchronize(remaining), error :: errors, statements)
  }
  val (errors, statements) = loop(input, List.empty, List.empty)
  Either.cond(errors.isEmpty, statements, ParseErrors(errors))


import TokenType.{ReservedWord, EOF, Semicolon}
import TokenType.ReservedWord.*
private def isSyncToken(t: TokenType) = t match
  case ReservedWord(`class`) => true
  case ReservedWord(`fun`) => true
  case ReservedWord(`var`) => true
  case ReservedWord(`for`) => true
  case ReservedWord(`if`) => true
  case ReservedWord(`while`) => true
  case ReservedWord(`print`) => true
  case ReservedWord(`return`) => true
  case EOF => true
  case Semicolon => true
  case _ => false

private def synchronize(input: List[Token]) =
  input.tail.dropWhile(t => !isSyncToken(t.`type`)).dropWhile(t => t.`type` == Semicolon)

private object Productions:
  import Expression.*
  import TokenType.*
  import Statement.*

  type ParserState[T] = CatsState[List[Token], Either[ParseError, T]]

  def declaration: ParserState[Statement] = CatsState { input =>
    input.headOption
      .map {
        case Token(ReservedWord(`var`)) => varDeclaration.run(input.tail).value
        case _ => statement.run(input).value
      }
      .getOrElse {
        (input, Left(ParseError.empty))
      }
  }

  def varDeclaration: ParserState[Statement] = CatsState { input =>
    input match // TODO unsafe
      case (name @ Token(Identifier(_))) :: Token(Semicolon) :: rest => (rest, Right(Statement.Var(name, None)))
      case (name @ Token(Identifier(_))) :: Token(Equal) :: rest =>
        val (remaining, initializer): (List[Token], Either[ParseError, Expression]) = expression.run(rest).value
        remaining.head match
          case Token(Semicolon) => (remaining.tail, initializer.map(init => Statement.Var(name, Some(init))))
          case t => (remaining, Left(ParseError("Expect ';' after variable declaration.", t)))
      case head :: rest => (input, Left(ParseError("Expect variable name.", head)))
      case _ => ???
  }

  def statement: ParserState[Statement] = CatsState { input =>
    input.headOption
      .map {
        case Token(ReservedWord(`for`)) => forStatement.run(input.tail).value
        case Token(ReservedWord(`if`)) => ifStatement.run(input.tail).value
        case Token(ReservedWord(`while`)) => whileStatement.run(input.tail).value
        case Token(LeftBracket) => block.map(b => b.map(Block(_))).run(input.tail).value
        case Token(ReservedWord(`print`)) => printStatement.run(input.tail).value
        case _ => expressionStatement.run(input).value
      }
      .getOrElse {
        (input, Left(ParseError.empty))
      }
  }

  def block: ParserState[List[Statement]] = CatsState { input =>
    def loop(input: List[Token], statements: Either[ParseError, List[Statement]]): (List[Token], Either[ParseError, List[Statement]]) = {
      input.headOption match
        case Some(Token(EOF)) => (input, statements.map(_.reverse))
        case Some(Token(RightBracket)) => (input, statements.map(_.reverse))
        case _ =>
          val (remaining, statement) = declaration.run(input).value
          loop(remaining, (statement, statements).mapN(_ :: _))
    }
    val (remaining, statements) = loop(input, Right(List.empty))
    remaining.headOption
      .map {
        case Token(RightBracket) => (remaining.tail, statements)
        case Token(EOF) => (remaining, statements)
        case t => (remaining, Left(ParseError("Expect '}' after block.", t)))
      }
      .getOrElse {
        (remaining, Left(ParseError.empty))
      }
  }

  def forStatement: ParserState[Statement] = CatsState { input =>
    input.headOption
      .map {
        case Token(LeftParenthesis) =>

          val (remaining, initializerOpt): (List[Token], Option[Either[ParseError, Statement]]) = input.tail.head match
            case Token(Semicolon) => (input.tail.tail, None)
            case Token(ReservedWord(`var`)) => varDeclaration.map(Some(_)).run(input.tail.tail).value
            case _ => expressionStatement.map(Some(_)).run(input.tail).value

          val (remaining2, conditionOpt): (List[Token], Option[Either[ParseError, Expression]]) = remaining.head match
            case Token(Semicolon) => (remaining, None)
            case _ => expression.map(Some(_)).run(remaining).value

          remaining2.head match {
            case Token(Semicolon) =>
              val (remaining3, incrementOpt): (List[Token], Option[Either[ParseError, Expression]]) = remaining2.tail.head match {
                case Token(RightParenthesis) => (remaining2.tail, None)
                case _ => expression.map(Some(_)).run(remaining2.tail).value
              }
              remaining3.head match {
                case Token(RightParenthesis) =>
                  statement.map { bodyE =>
                    val conditionE = conditionOpt.getOrElse(Right(Literal(true)))
                    val bodyIncE = incrementOpt.fold(bodyE) { incrementE =>
                      (bodyE, incrementE).mapN((body, increment) => Block(List(body, Expr(increment))))
                    }
                    val whileE = (conditionE, bodyIncE).mapN { (cond, body) =>
                      While(cond, Block(List(body)))
                    }
                    initializerOpt.fold(whileE) { initializerE =>
                      (initializerE, whileE).mapN(
                        (init: Statement, w: Statement) => Block(List(init, w)))
                    }
                  }.run(remaining3.tail).value
                case _ if (incrementOpt.map(_.isLeft).getOrElse(false)) => (remaining3, Left(incrementOpt.get.left.get)) // TODO cleanup
                case t => (remaining2, Left(ParseError("Expect ')' after for clauses.", t)))
              }
            case _ if (initializerOpt.map(_.isLeft).getOrElse(false)) => (remaining2, Left(initializerOpt.get.left.get)) // TODO cleanup
            case _ if (conditionOpt.map(_.isLeft).getOrElse(false)) => (remaining2, Left(conditionOpt.get.left.get)) // TODO cleanup
            case t => (remaining2, Left(ParseError("Expect ';' after loop condition.", t)))
          }
        case t => (input, Left(ParseError("Expect '(' after 'for'.", t)))
      }
      .getOrElse {
        (input, Left(ParseError.empty))
      }
  }

  def whileStatement: ParserState[Statement] = CatsState { input =>
    input.headOption
      .map {
        case Token(LeftParenthesis) =>
          val (remaining, condition) = expression.run(input.tail).value
          remaining.head match
            case Token(RightParenthesis) =>
              statement.map(stmt => (condition, stmt).mapN {
                While(_, _)
              }).run(remaining.tail).value
            case t => (input, Left(ParseError("Expect ')' after while condition.", t)))
        case t => (input, Left(ParseError("Expect '(' after 'while'.", t)))
      }
      .getOrElse {
        (input, Left(ParseError.empty))
      }
  }

  def ifStatement: ParserState[Statement] = CatsState { input =>
    input.headOption
      .map {
        case Token(LeftParenthesis) =>
          val (remaining, expr) = expression.run(input.tail).value
          remaining.head match
            case Token(RightParenthesis) =>
              val (remaining2, thenBranch) = statement.run(remaining.tail).value
              remaining2.head match
                case Token(ReservedWord(`else`)) =>
                  statement.map {
                    elseBranch =>
                      (expr, thenBranch, elseBranch).mapN { (expr, thenBranch, elseBranch) =>
                        If(expr, thenBranch, Some(elseBranch))
                      }
                  }.run(remaining2.tail).value
                case _ => (remaining2, (expr, thenBranch).mapN(Statement.If(_, _, None)))
            case t => (input, Left(ParseError("Expect ')' after if condition.", t)))
        case t => (input, Left(ParseError("Expect '(' after 'if'.", t)))
      }
      .getOrElse {
        (input, Left(ParseError.empty))
      }
  }

  def printStatement: ParserState[Statement] = CatsState { input =>
    val (remaining, exprE): (List[Token], Either[ParseError, Expression]) = expression.run(input).value
    exprE match
      case Right(expr) =>
        remaining.headOption
          .map {
            case Token(Semicolon) => (remaining.tail, Right(Statement.Print(expr)))
            case t => (remaining, Left(ParseError("Expect ';' after value.", t)))
          }
          .getOrElse {
            (remaining, Left(ParseError.empty))
          }
      case Left(error) => (remaining, Left(error))
  }

  def expressionStatement: ParserState[Statement] = CatsState { input =>
    val (remaining, exprE): (List[Token], Either[ParseError, Expression]) = expression.run(input).value
    exprE match
      case Right(expr) =>
        remaining.headOption.map {
          case Token(Semicolon) => (remaining.tail, Right(Statement.Expr(expr)))
          case t => (remaining, Left(ParseError("Expect ';' after expression.", t)))
        }
        .getOrElse {
          (remaining, Left(ParseError.empty))
        }
      case Left(error) => (remaining, Left(error))
  }

  def expression: ParserState[Expression] = assignment

  def or: ParserState[Expression] = CatsState { input =>
    val (remaining, left) = and.run(input).value
    @tailrec
    def loop(input: List[Token], left: Either[ParseError, Expression]): (List[Token], Either[ParseError, Expression]) = {
      input.headOption match {
        case Some(token) =>
          token.`type` match
            case ReservedWord(ReservedWords.`or`) =>
              val (i, right) = and.run(input.tail).value
              loop(i, (left, right).mapN(Logical(_, token, _)))
            case _ => (input, left)
        case None => (input, Left(ParseError.empty))
      }
    }
    loop(remaining, left)
  }

  def and: ParserState[Expression] = CatsState { input =>
    val (remaining, left) = equality.run(input).value
    @tailrec
    def loop(input: List[Token], left: Either[ParseError, Expression]): (List[Token], Either[ParseError, Expression]) = {
      input.headOption match {
        case Some(token) =>
          token.`type` match
            case ReservedWord(ReservedWords.`and`) =>
              val (i, right) = equality.run(input.tail).value
              loop(i, (left, right).mapN(Logical(_, token, _)))
            case _ => (input, left)
        case None => (input, Left(ParseError.empty))
      }
    }
    loop(remaining, left)
  }

  def assignment: ParserState[Expression] = CatsState { input =>
    val (out, left): (List[Token], Either[ParseError, Expression]) = or.run(input).value
    left match {
      case Right(expr) =>
        out.headOption
          .map {
            case t @ Token(Equal) => expr match
              case Expression.Var(name) => assignment.map(value => value.map(Assign(name, _))).run(out.tail).value
              case _ => (out, Left(ParseError("Invalid assignment target.", t)))
            case _ => (out, left)
          }
          .getOrElse {
            (out, Left(ParseError.empty))
          }
      case _ => (out, left)
    }
  }

  type Equality = EqualEqual.type | BangEqual.type
  type Comparison = GreaterThan.type | GreaterThanEqual.type | LessThan.type | LessThanEqual.type
  type Term = Minus.type | Plus.type
  type Factor = Star.type | Slash.type
  type Unary = Bang.type | Minus.type

  def equality = binaryMatch[Equality](comparison)
  def comparison = binaryMatch[Comparison](term)
  def term = binaryMatch[Term](factor)
  def factor = binaryMatch[Factor](unary)

  inline def binaryMatch[A <: TokenType](next: ParserState[Expression]): ParserState[Expression] = CatsState { input =>
    val (in, left): (List[Token], Either[ParseError, Expression]) = next.run(input).value
    @tailrec
    def loop(input: List[Token], left: Either[ParseError, Expression]): (List[Token], Either[ParseError, Expression]) = {
      input.headOption match {
        case Some(token) =>
          token.`type` match
            case t: A =>
              val (i, right) = next.run(input.tail).value
              loop(i, (left, right).mapN(Binary(_, token, _)))
            case _ => (input, left)
        case None => (input, Left(ParseError.empty))
      }
    }
    loop(in, left)
  }

  def unary: ParserState[Expression] = CatsState { input =>
    input.headOption
      .map { token =>
        token.`type` match
          case t: Unary =>
            unary.map(right => right.map(Unary(token, _))).run(input.tail).value
          case _ => primary.run(input).value
      }
      .getOrElse {
        (input, Left(ParseError.empty))
      }
  }

  def primary: ParserState[Expression] = CatsState { input =>
    input.headOption
      .map { token =>
        token.`type` match
          case TokenType.String(lexeme) => (input.tail, Right(Literal(lexeme)))
          case TokenType.Number(lexeme) => (input.tail, Right(Literal(lexeme.toDouble)))
          case Identifier(_) => (input.tail, Right(Expression.Var(token)))
          case ReservedWord(`true`) => (input.tail, Right(Literal(true)))
          case ReservedWord(`false`) => (input.tail, Right(Literal(false)))
          case ReservedWord(`nil`) => (input.tail, Right(Literal(null)))
          case LeftParenthesis =>
            val (i, expr) = expression.run(input.tail).value
            i.head match
              case Token(RightParenthesis) => (i.tail, expr.map(Grouping(_)))
              case t => (input, Left(ParseError("Expected ')' after expression", token)))
          case t => (input, Left(ParseError("Expect expression.", token)))
      }
      .getOrElse {
        (input, Left(ParseError.empty))
      }

  }