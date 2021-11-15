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
  def show: String = this match {
    case Assign(name, expr) => s"(${name.lexeme} = ${expr.show})"
    case Binary(left, operator, right) => s"(${operator.lexeme} ${left.show} ${right.show})"
    case Unary(operator, right) => s"(${operator.lexeme} ${right.show})"
    case Grouping(expr) => s"(group ${expr.show})"
    case l: Literal[?] => ExprResult.from(l.value).show
    case Var(name) => s"`${name.lexeme}`"
  }
  case Assign(name: Token, expr: Expression)
  case Binary(left: Expression, operator: Token, right: Expression)
  case Logical(left: Expression, operator: Token, right: Expression)
  case Unary(operator: Token, right: Expression)
  case Grouping(expr: Expression)
  case Literal[T <: ExprValue ](value: T)
  case Var(name: Token)

def parse(input: List[Token]): Either[ParseErrors, List[Statement]] =
  @tailrec
  def loop(input: List[Token], errors: List[ParseError], statements: List[Statement]): (List[ParseError], List[Statement]) = {
    input.headOption match
      case Some(Token(TokenType.EOF)) => (errors.reverse, statements.reverse)
      case _ =>
        Try(Productions.declaration(input)) match
          case Success((remaining, statement)) => loop(remaining, errors, statement :: statements)
          case Failure(e: ParseError) => // TODO do better than stuffing this into `e`
            val remaining = synchronize(e.tail)
            loop(remaining, e :: errors, statements)
  }
  val (errors, statements) = loop(input, List.empty, List.empty)
  Either.cond(errors.isEmpty, statements, ParseErrors(errors))

/**
 * TODO docblock
 * @param t
 * @return
 */
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
  type Equality = EqualEqual.type | BangEqual.type
  type Comparison = GreaterThan.type | GreaterThanEqual.type | LessThan.type | LessThanEqual.type
  type Term = Minus.type | Plus.type
  type Factor = Star.type | Slash.type
  type Unary = Bang.type | Minus.type

  inline def binaryMatch[A <: TokenType](next: List[Token] => (List[Token], Expression))(input: List[Token]) =
    val (in, left) = next(input)
    @tailrec
    def loop(input: List[Token], expr: Expression): (List[Token], Expression) = {
      // TODO unsafe
      val token = input.head
      token.`type` match
        case t : A =>
          val (i, right) = next(input.drop(1))
          loop(i, Binary(expr, token, right))
        case _ => (input, expr)
    }
    loop(in, left)

  def declaration(input: List[Token]): (List[Token], Statement) =
    input.head match
      case Token(ReservedWord(`var`)) => varDeclaration(input.tail)
      case _ => statement(input)

  def varDeclaration(input: List[Token]) =
    // TODO unsafe
    input match
      case (name @ Token(Identifier(_))) :: Token(Semicolon) :: rest => (rest, Statement.Var(name, None))
      case (name @ Token(Identifier(_))) :: Token(Equal) :: rest =>
        val (remaining, initializer) = expression(rest)
        remaining.head match
          case Token(Semicolon) => (remaining.tail, Statement.Var(name, Some(initializer)))
          case t => throw ParseError("Expect ';' after variable declaration.", t, remaining)
      case head :: rest => throw ParseError("Expect variable name.", head, input)
      case _ => ???

  def statement(input: List[Token]): (List[Token], Statement) =
    // TODO unsafe
    input.head match
      case Token(ReservedWord(`for`)) => forStatement(input.tail)
      case Token(ReservedWord(`if`)) => ifStatement(input.drop(1))
      case Token(ReservedWord(`while`)) => whileStatement(input.drop(1))
      case Token(LeftBracket) =>
        val (remaining, statements) = block(input.drop(1))
        (remaining, Statement.Block(statements))
      case Token(ReservedWord(`print`)) => printStatement(input.drop(1))
      case _ => expressionStatement(input)

  def block(input: List[Token]): (List[Token], List[Statement]) =
    def loop(input: List[Token], statements: List[Statement]): (List[Token], List[Statement]) = {
      input.headOption match
        case Some(Token(EOF)) => (input, statements.reverse)
        case Some(Token(RightBracket)) => (input, statements.reverse)
        case _ =>
          val (remaining, statement) = declaration(input)
          loop(remaining, statement :: statements)
    }
    val (remaining, statements) = loop(input, List.empty)
    remaining.head match
      case Token(RightBracket) => (remaining.tail, statements)
      case Token(EOF) => (remaining, statements)
      case t => throw new ParseError("Expect '}' after block.", t, remaining)

  def forStatement(input: List[Token]) =
    input.head match
      case Token(LeftParenthesis) =>
        val (remaining, initializerOpt) = input.tail.head match
          case Token(Semicolon) =>
            (input.tail.drop(1), None)
          case Token(ReservedWord(`var`)) => varDeclaration(input.tail.drop(1)) match
            case (remaining, statement) =>
              (remaining, Some(statement))
          case _ => expressionStatement(input.tail) match
            case (remaining, statement) =>
              (remaining, Some(statement))
        val (remaining2, conditionOpt) = remaining.head match
          case Token(Semicolon) => (remaining, None)
          case _ => expression(remaining) match
            case (remaining, expression) =>
              (remaining, Some(expression))
        remaining2.head match {
          case Token(Semicolon) =>
            val (remaining3, incrementOpt) = remaining2.tail.head match {
              case Token(RightParenthesis) => (remaining2.tail, None)
              case _ => expression(remaining2.tail) match
                case (remaining, expression) => (remaining, Some(expression))
            }
            remaining3.head match
              case Token(RightParenthesis) =>
                val (remaining4, body) = statement(remaining3.tail)
                val fullBody = incrementOpt.fold(body) { increment =>
                  Statement.Block {
                    List(body, Statement.Expr(increment))
                  }
                }
                val condition = conditionOpt.getOrElse(Literal(true))
                val whileStatement = Statement.While(condition, fullBody)
                val finalStatement = initializerOpt.fold(whileStatement) { initializer =>
                  Statement.Block(List(initializer, whileStatement))
                }
                (remaining4, finalStatement)
              case t => throw new ParseError("Expect ')' after for clauses.", t, remaining2)
          case t => throw new ParseError("Expect ';' after loop condition.", t, remaining2)
        }
      case t => throw new ParseError("Expect '(' after 'for'.", t, input)

  def whileStatement(input: List[Token]) =
    input.head match
      case Token(LeftParenthesis) =>
        val (remaining, condition) = expression(input.tail)
          remaining.head match
            case Token(RightParenthesis) =>
              val (remaining2, body) = statement(remaining.tail)
              (remaining2, Statement.While(condition, body))
            case t => throw new ParseError("Expect ')' after while condition.", t, input)
      case t => throw new ParseError("Expect '(' after 'while'.", t, input)

  def ifStatement(input: List[Token]) =
    input.head match
      case Token(LeftParenthesis) =>
        val (remaining, expr) = expression(input.tail)
        remaining.head match
          case Token(RightParenthesis) =>
            val (remaining2, thenBranch) = statement(remaining.tail)
            remaining2.head match
              case Token(ReservedWord(`else`)) =>
                val (remaining3, elseBranch) = statement(remaining2.tail)
                (remaining3, Statement.If(expr, thenBranch, Some(elseBranch)))
              case _ => (remaining2, Statement.If(expr, thenBranch, None))
          case t => throw new ParseError("Expect ')' after if condition.", t, input)
      case t => throw new ParseError("Expect '(' after 'if'.", t, input)

  def printStatement(input: List[Token]) =
    val (remaining, expr) = expression(input)
    // TODO unsafe
    remaining.head match
      case Token(Semicolon) => (remaining.drop(1), Statement.Print(expr))
      case t => throw ParseError("Expect ';' after value.", t, remaining)

  def expressionStatement(input: List[Token]) =
    val (remaining, expr) = expression(input)
    // TODO unsafe
    remaining.head match
      case Token(Semicolon) => (remaining.drop(1), Statement.Expr(expr))
      case t => throw ParseError("Expect ';' after expression.", t, remaining)

  def expression(input: List[Token]) = assignment(input)

  // TODO unify with binaryMatch
  def or(input: List[Token]) =
    val (remaining, left) = and(input)
    @tailrec
    def loop(input: List[Token], expr: Expression): (List[Token], Expression) = {
      // TODO unsafe
      val token = input.head
      token.`type` match
        case ReservedWord(ReservedWords.`or`) =>
          val (i, right) = and(input.drop(1))
          loop(i, Logical(expr, token, right))
        case _ => (input, expr)
    }
    loop(remaining, left)

  def and(input: List[Token]) =
    val (remaining, left) = equality(input)
    @tailrec
    def loop(input: List[Token], expr: Expression): (List[Token], Expression) = {
      // TODO unsafe
      val token = input.head
      token.`type` match
        // TODO fix scanner hack
        case ReservedWord(ReservedWords.`and`) =>
          val (i, right) = equality(input.drop(1))
          loop(i, Logical(expr, token, right))
        case _ => (input, expr)
    }
    loop(remaining, left)

  def assignment(input: List[Token]): (List[Token], Expression) =
    val (out, expr) = or(input)
    out.head match
      case t @ Token(Equal) =>
        expr match
          case Var(name) =>
            val (tail, value) = assignment(out.tail)
            (tail, Assign(name, value))
          case _ =>
            // TODO not supposed to throw here, just report
            // https://github.com/munificent/craftinginterpreters/blob/master/java/com/craftinginterpreters/lox/Lox.java#L109-L117
            throw ParseError("Invalid assignment target.", t, out)
      case _ => (out, expr)

  def equality(input: List[Token])   = binaryMatch[Equality](comparison _)(input)
  def comparison(input: List[Token]) = binaryMatch[Comparison](term _)(input)
  def term(input: List[Token])       = binaryMatch[Term](factor _)(input)
  def factor(input: List[Token])     = binaryMatch[Factor](unary _)(input)

  def unary(input: List[Token]): (List[Token], Expression) =
    // TODO unsafe
    val token = input.head
    token.`type` match
      case t : Unary =>
        val (in, right) = unary(input.drop(1))
        (in, Unary(token, right))
      case _ => primary(input)

  def primary(input: List[Token]): (List[Token], Expression) =
    // TODO unsafe
    val token = input.head
    token.`type` match
      case TokenType.String(lexeme) => (input.drop(1), Literal(lexeme))
      case TokenType.Number(lexeme) => (input.drop(1), Literal(lexeme.toDouble))
      case Identifier(_) => (input.drop(1), Var(token))
      case ReservedWord(`true`) => (input.drop(1), Literal(true))
      case ReservedWord(`false`) => (input.drop(1), Literal(false))
      case ReservedWord(`nil`) => (input.drop(1), Literal(null))
      case LeftParenthesis =>
        val (i, expr) = expression(input.drop(1))
        i.head match
          case Token(RightParenthesis) => (i.drop(1), Grouping(expr))
          case t => throw ParseError("Expected ')' after expression", token, input)
      case t => throw ParseError("Expect expression.", token, input)