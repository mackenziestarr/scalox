import ReservedWords.*
import scala.annotation.tailrec
import scala.util.Try

enum Statement:
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
        val (remaining, result) = Productions.declaration(input)
        result match
          case statement: Statement => loop(remaining, errors, statement :: statements)
          case error: ParseError => loop(remaining, error :: errors, statements)
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

  def declaration(input: List[Token]): (List[Token], Statement | ParseError) =
    try {
      input.head match {
        case Token(ReservedWord(`var`)) => varDeclaration(input.tail)
        case _ => statement(input)
      }
    } catch {
      case (e: ParseError) =>
        // TODO do better than stuffing this into `e`
        val remaining = synchronize(e.tail)
        (remaining, e)
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

  def statement(input: List[Token]) =
    // TODO unsafe
    input.head match
      case Token(ReservedWord(`print`)) => printStatement(input.drop(1))
      case _ => expressionStatement(input)

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
      case t : TokenType.String => (input.drop(1), Literal(t.value))
      // TODO move double conversion here
      case t : TokenType.Number => (input.drop(1), Literal(t.value))
      case _ : Identifier => (input.drop(1), Var(token))
      case ReservedWord(`true`) => (input.drop(1), Literal(true))
      case ReservedWord(`false`) => (input.drop(1), Literal(false))
      case ReservedWord(`nil`) => (input.drop(1), Literal(null))
      case LeftParenthesis =>
        val (i, expr) = expression(input.drop(1))
        i.head match
          case Token(RightParenthesis) => (i.drop(1), Grouping(expr))
          case t => throw ParseError("Expected ')' after expression", token, input)
      case t => throw ParseError("Expect expression.", token, input)