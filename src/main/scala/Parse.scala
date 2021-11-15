import ReservedWords.*

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

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
      case Some(Token(TokenType.EOF)) => (errors.reverse, statements.reverse)
      case _ =>
        Try(Productions.declaration.run(input)) match
          case Success((statement, remaining)) => loop(remaining, errors, statement :: statements)
          case Failure(e: ParseError) => // TODO do better than stuffing this into `e`
            val remaining = synchronize(e.tail)
            loop(remaining, e :: errors, statements)
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

  def declaration: State[List[Token], Statement] = State { input =>
    input.head match // TODO unsafe
      case Token(ReservedWord(`var`)) => varDeclaration.run(input.tail)
      case _ => statement.run(input)
  }

  def varDeclaration: State[List[Token], Statement] = State { input =>
    input match // TODO unsafe
      case (name @ Token(Identifier(_))) :: Token(Semicolon) :: rest => (Statement.Var(name, None), rest)
      case (name @ Token(Identifier(_))) :: Token(Equal) :: rest =>
        val (initializer, remaining) = expression.run(rest)
        remaining.head match
          case Token(Semicolon) => (Statement.Var(name, Some(initializer)), remaining.tail)
          case t => throw ParseError("Expect ';' after variable declaration.", t, remaining)
      case head :: rest => throw ParseError("Expect variable name.", head, input)
      case _ => ???
  }

  def statement: State[List[Token], Statement] = State { input =>
    input.head match // TODO unsafe
      case Token(ReservedWord(`for`)) => forStatement.run(input.tail)
      case Token(ReservedWord(`if`)) => ifStatement.run(input.tail)
      case Token(ReservedWord(`while`)) => whileStatement.run(input.tail)
      case Token(LeftBracket) => block.map(Block(_)).run(input.tail)
      case Token(ReservedWord(`print`)) => printStatement.run(input.tail)
      case _ => expressionStatement.run(input)
  }

  def block: State[List[Token], List[Statement]] = State { input =>
    def loop(input: List[Token], statements: List[Statement]): (List[Token], List[Statement]) = {
      input.headOption match
        case Some(Token(EOF)) => (input, statements.reverse)
        case Some(Token(RightBracket)) => (input, statements.reverse)
        case _ =>
          val (statement, remaining) = declaration.run(input)
          loop(remaining, statement :: statements)
    }
    val (remaining, statements) = loop(input, List.empty)
    remaining.head match // TODO unsafe
      case Token(RightBracket) => (statements, remaining.tail)
      case Token(EOF) => (statements, remaining)
      case t => throw new ParseError("Expect '}' after block.", t, remaining)
  }

  def forStatement: State[List[Token], Statement] = State { input =>
    input.head match { // TODO unsafe
      case Token(LeftParenthesis) =>

        val (initializerOpt, remaining) = input.tail.head match
          case Token(Semicolon) => (None, input.tail.tail)
          case Token(ReservedWord(`var`)) => varDeclaration.map(Some(_)).run(input.tail.tail)
          case _ => expressionStatement.map(Some(_)).run(input.tail)

        val (conditionOpt, remaining2) = remaining.head match
          case Token(Semicolon) => (None, remaining)
          case _ => expression.map(Some(_)).run(remaining)

        remaining2.head match {
          case Token(Semicolon) =>

            val (incrementOpt, remaining3) = remaining2.tail.head match {
              case Token(RightParenthesis) => (None, remaining2.tail)
              case _ => expression.map(Some(_)).run(remaining2.tail)
            }

            remaining3.head match {
              case Token(RightParenthesis) =>
                statement.map { body =>
                  val whileStatement = While(
                    conditionOpt.getOrElse(Literal(true)),
                    incrementOpt.fold(body) { increment =>
                      Block(List(body, Expr(increment)))
                    }
                  )
                  initializerOpt.fold(whileStatement) { initializer =>
                    Block(List(initializer, whileStatement))
                  }
                }.run(remaining3.tail)
              case t => throw new ParseError("Expect ')' after for clauses.", t, remaining2)
            }
          case t => throw new ParseError("Expect ';' after loop condition.", t, remaining2)
        }
      case t => throw new ParseError("Expect '(' after 'for'.", t, input)
    }
  }

  def whileStatement: State[List[Token], Statement] = State { input =>
    input.head match // TODO unsafe
      case Token(LeftParenthesis) =>
        val (condition, remaining) = expression.run(input.tail)
        remaining.head match
          case Token(RightParenthesis) => statement.map(While(condition, _)).run(remaining.tail)
          case t => throw new ParseError("Expect ')' after while condition.", t, input)
      case t => throw new ParseError("Expect '(' after 'while'.", t, input)
  }

  def ifStatement: State[List[Token], Statement] = State { input =>
    input.head match // TODO unsafe
      case Token(LeftParenthesis) =>
        val (expr, remaining) = expression.run(input.tail)
        remaining.head match
          case Token(RightParenthesis) =>
            val (thenBranch, remaining2) = statement.run(remaining.tail)
            remaining2.head match
              case Token(ReservedWord(`else`)) =>
                statement.map(elseBranch => If(expr, thenBranch, Some(elseBranch))).run(remaining2.tail)
              case _ => (Statement.If(expr, thenBranch, None), remaining2)
          case t => throw new ParseError("Expect ')' after if condition.", t, input)
      case t => throw new ParseError("Expect '(' after 'if'.", t, input)
  }

  def printStatement: State[List[Token], Statement] = State { input =>
    val (expr, remaining) = expression.run(input)
    // TODO unsafe
    remaining.head match
      case Token(Semicolon) => (Statement.Print(expr), remaining.tail)
      case t => throw ParseError("Expect ';' after value.", t, remaining)
  }

  def expressionStatement: State[List[Token], Statement] = State { input =>
    val (expr, remaining) = expression.run(input)
    remaining.head match // TODO unsafe
      case Token(Semicolon) => (Statement.Expr(expr), remaining.tail)
      case t => throw ParseError("Expect ';' after expression.", t, remaining)
  }

  def expression: State[List[Token], Expression] = assignment

  // TODO unify with binaryMatch
  def or: State[List[Token], Expression] = State { input =>
    val (left, remaining) = and.run(input)
    @tailrec
    def loop(expr: Expression, input: List[Token]): (Expression, List[Token]) = {
      // TODO unsafe
      val token = input.head
      token.`type` match
        case ReservedWord(ReservedWords.`or`) =>
          val (right, i) = and.run(input.tail)
          loop(Logical(expr, token, right), i)
        case _ => (expr, input)
    }
    loop(left, remaining)
  }

  def and: State[List[Token], Expression] = State { input =>
    val (left, remaining) = equality.run(input)
    @tailrec
    def loop(expr: Expression, input: List[Token]): (Expression, List[Token]) = {
      // TODO unsafe
      val token = input.head
      token.`type` match
        // TODO fix scanner hack
        case ReservedWord(ReservedWords.`and`) =>
          val (right, i) = equality.run(input.tail)
          loop(Logical(expr, token, right), i)
        case _ => (expr, input)
    }
    loop(left, remaining)
  }

  def assignment: State[List[Token], Expression] = State { input =>
    val (expr, out) = or.run(input)
    out.head match
      case t @ Token(Equal) =>
        expr match
          case Expression.Var(name) => assignment.map(value => Assign(name, value)).run(out.tail)
          case _ =>
            // TODO not supposed to throw here, just report
            // https://github.com/munificent/craftinginterpreters/blob/master/java/com/craftinginterpreters/lox/Lox.java#L109-L117
            throw ParseError("Invalid assignment target.", t, out)
      case _ => (expr, out)
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

  inline def binaryMatch[A <: TokenType](next: State[List[Token], Expression]): State[List[Token], Expression] = State { input =>
    val (left, in) = next.run(input)
    @tailrec
    def loop(expr: Expression, input: List[Token]): (Expression, List[Token]) = {
      val token = input.head // TODO unsafe
      token.`type` match
        case t: A =>
          val (right, i) = next.run(input.tail)
          loop(Binary(expr, token, right), i)
        case _ => (expr, input)
    }
    loop(left, in)
  }

  def unary: State[List[Token], Expression] = State { input =>
    val token = input.head
    token.`type` match
      case t : Unary =>
        unary.map(right => Unary(token, right)).run(input.tail)
      case _ => primary.run(input)
  }

  def primary: State[List[Token], Expression] = State { input =>
    val token = input.head // TODO unsafe
    token.`type` match
      case TokenType.String(lexeme) => (Literal(lexeme), input.tail)
      case TokenType.Number(lexeme) => (Literal(lexeme.toDouble), input.tail)
      case Identifier(_) => (Expression.Var(token), input.tail)
      case ReservedWord(`true`) => (Literal(true), input.tail)
      case ReservedWord(`false`) => (Literal(false), input.tail)
      case ReservedWord(`nil`) => (Literal(null), input.tail)
      case LeftParenthesis =>
        val (expr, i) = expression.run(input.tail)
        i.head match
          case Token(RightParenthesis) => (Grouping(expr), i.tail)
          case t => throw ParseError("Expected ')' after expression", token, input)
      case t => throw ParseError("Expect expression.", token, input)
  }