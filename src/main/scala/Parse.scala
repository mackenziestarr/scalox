import ReservedWords.*

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

enum Statement:
  case Block(statements: List[Statement])
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
        Try(Productions.declaration(input)) match {
          case Success((remaining, statement)) => loop(remaining, errors, statement :: statements)
          case Failure(e: ParseError) => // TODO do better than stuffing this into `e`
            val remaining = synchronize(e.tail)
            loop(remaining, e :: errors, statements)
        }
  }
  val (errors, statements) = loop(input, List.empty, List.empty)
  Either.cond(errors.isEmpty, statements, ParseErrors(errors))

/**
 * TODO docblock
 * @param t
 * @return
 */
import TokenType.{ReservedWord, EOF}
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
  case _ => false

private def synchronize(input: List[Token]) = input.dropWhile(t => !isSyncToken(t.`type`))

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
    input.head match {
      case Token(ReservedWord(`var`)) => varDeclaration(input.tail)
      case _ => statement(input)
    }

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
      case Token(LeftBracket) =>
        val (remaining, statements) = block(input.drop(1))
        (remaining, Statement.Block(statements))
      case Token(ReservedWord(`print`)) => printStatement(input.drop(1))
      case _ => expressionStatement(input)

  def block(input: List[Token]): (List[Token], List[Statement]) =
    @tailrec
    def loop(input: List[Token], statements: List[Statement]): (List[Token], List[Statement]) = {
      input.headOption match {
        case Some(Token(EOF)) => (input, statements.reverse)
        case Some(Token(RightBracket)) => (input, statements.reverse)
        case _ =>
          val (remaining, statement) = declaration(input)
          loop(remaining, statement :: statements)
      }
    }
    val (remaining, statements) = loop(input, List.empty)
    remaining.head match
      case Token(RightBracket) => (remaining.tail, statements)
      case Token(EOF) => (remaining, statements)
      case t => throw new ParseError("Expect '}' after block.", t, remaining)



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

  def assignment(input: List[Token]): (List[Token], Expression) =
    val (out, expr) = equality(input)
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