import scala.annotation.tailrec
import scala.collection.StringView
import scala.util.Try

// TODO: List or Vector pick one!
def scan(in: String): Either[ScanErrors, List[Token]] =
  scan(in, 1, Vector.empty, Vector.empty)

@tailrec
private def scan(in: String, line: Int, errors: Vector[ScanError], tokens: Vector[Token]): Either[ScanErrors, List[Token]] =
  if in.isEmpty then 
    val eof = Token("EOF", TokenType.EOF, line)
    Either.cond(errors.isEmpty, tokens.appended(eof).toList, ScanErrors(errors.toList))
  else
    val (n, inc, errorOpt, tokenOpt) = token(in, line) match {
      case t : TokenType.String   => (t.lexeme.length + 2, t.lexeme.count(_ == '\n'), None, Some(Token(t.lexeme, t, line)))
      case t : TokenType          => (t.lexeme.length, 0, None, Some(Token(t.lexeme, t, line)))
      case Skip(length, newLines) => (length, newLines, None, None)
      case s : ScanError => (s.drop, 0, Some(s), None)
    }
    scan(in.drop(n), line + inc, errors ++ errorOpt, tokens ++ tokenOpt)

private def token(in: String, line: Int): ScanError | Skip | TokenType =
  import TokenType._
  val (head, tail) = (in.head, in.tail)
  head match
    case '(' => LeftParenthesis
    case ')' => RightParenthesis
    case '{' => LeftBracket
    case '}' => RightBracket
    case ',' => Comma
    case '.' => Dot
    case '+' => Plus
    case '-' => Minus
    case ';' => Semicolon
    case '*' => Star
    case '!' => if tail.headOption == Some('=') then BangEqual else Bang
    case '=' => if tail.headOption == Some('=') then EqualEqual else Equal
    case '<' => if tail.headOption == Some('=') then LessThanEqual else LessThan
    case '>' => if tail.headOption == Some('=') then GreaterThanEqual else GreaterThan
    case '/' => tail.headOption match
      case Some('/') => Skip(in.takeWhile(_ != '\n').length, 0)
      case Some('*') => parseMultiLine(in, line)
      case _ => Slash
    case ' ' | '\r' | '\t' => Skip(1, 0)
    case '\n' => Skip(1, 1)
    case '"' => parseString(in, line)
    case x if isDigit(x) => parseNumber(in, line)
    case x if isAlpha(x) => parseIdentifier(in)
    case _ => ScanError(s"Unexpected character: '$head'", line, 1)

private def parseMultiLine(in: String, line: Int): ScanError | Skip =
  parseMultiLine(in.drop(2), line, "/*", 1, false)
@tailrec
private def parseMultiLine(in: String, line: Int, drop: String, nestingLevel: Int, hadError: Boolean): ScanError | Skip =
  if nestingLevel == 0 then
    if hadError
    then ScanError("Unterminated multi-line comment.", line, in.length + drop.length)
    else Skip(drop.length, drop.count(_ == '\n'))
  else
    in.indexOf("/*") match
      case -1 => in.indexOf("*/") match
        case -1 => parseMultiLine(in, line, "", 0, true)
        case i => parseMultiLine(in.drop(i + 2), line, drop + in.substring(0, i) + "*/", nestingLevel - 1, false)
      case i => parseMultiLine(in.drop(i + 2), line, drop + in.substring(0, i) + "/*", nestingLevel + 1, false)

private def parseString(in: String, line: Int): ScanError | TokenType =
  val (str, rest) = in.tail.span(_ != '"')
  if rest.headOption == Some('"') then
    TokenType.String(str, str)
  else ScanError("Unterminated string.", line, in.length)

private def isDigit(c: Char): Boolean = ('0' to '9').containsTyped(c)
private def isAlpha(c: Char): Boolean = ('a' to 'z').containsTyped(c) || ('A' to 'Z').containsTyped(c) || c == '_'
private def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

private def parseNumber(in: String, line: Int): ScanError | TokenType =
  val (prefix, rest) = in.span(isDigit)
  val suffix: String = if rest.headOption == Some('.') then "." + rest.tail.takeWhile(isDigit) else ""
  val lexeme = prefix + suffix
  if (suffix == ".") then
    ScanError(s"Invalid numeric syntax, found '${prefix}.' expected '${prefix}.0'", line, lexeme.length)
  else
    TokenType.Number(lexeme, lexeme.toDouble)

private def parseIdentifier(in: String) =
  val word = in.takeWhile(isAlphaNumeric)
  Try(ReservedWords.valueOf(word))
    .toOption
    .fold(TokenType.Identifier(word)) {
      TokenType.ReservedWord(word, _)
    }

/**
 * used for consuming tokens not considered in parser
 * e.g. newlines, whitespace, comments
 * @param length to advance to next token
 * @param newLines present in the consumed contents
 */
private case class Skip(length: Int, newLines: Int)

case class Token(lexeme: String, `type`: TokenType, line: Int)
object Token:
  def unapply(t: Token) = Some(t.`type`)

enum TokenType(val lexeme: String):
  case LeftParenthesis extends TokenType("(")
  case RightParenthesis extends TokenType(")")
  case LeftBracket extends TokenType("{")
  case RightBracket extends TokenType("}")
  case Comma extends TokenType(",")
  case Dot extends TokenType(".")
  case Minus extends TokenType("-")
  case Plus extends TokenType("+")
  case Semicolon extends TokenType(";")
  case Slash extends TokenType("/")
  case Star extends TokenType("*")
  case Bang extends TokenType("!")
  case BangEqual extends TokenType("!=")
  case Equal extends TokenType("=")
  case EqualEqual extends TokenType("==")
  case LessThanEqual extends TokenType("<=")
  case LessThan extends TokenType("<")
  case GreaterThanEqual extends TokenType(">=")
  case GreaterThan extends TokenType(">")
  case String(override val lexeme: Predef.String, value: Predef.String) extends TokenType(lexeme)
  case Number(override val lexeme: Predef.String, value: Double) extends TokenType(lexeme)
  case ReservedWord(override val lexeme: Predef.String, `type`: ReservedWords) extends TokenType(lexeme)
  case Identifier(override val lexeme: Predef.String) extends TokenType(lexeme)
  case EOF extends TokenType("EOF")

enum ReservedWords:
  case `and`
  case `class`
  case `else`
  case `false`
  case `for`
  case `fun`
  case `if`
  case `nil`
  case `or`
  case `print`
  case `return`
  case `super`
  case `this`
  case `true`
  case `var`
  case `while`