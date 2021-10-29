package io.lox
import scala.annotation.tailrec
import scala.collection.StringView
import scala.util.Try

case class ScanError(message: String, line: Int, drop: Int):
  def show: String = s"[line ${line}] Error: ${message}"

def scan(in: String): Either[Vector[ScanError], Vector[Token]] =
  scan(in, 1, Vector.empty, Vector.empty)

@tailrec
private def scan(in: String, line: Int, errors: Vector[ScanError], tokens: Vector[Token]): Either[Vector[ScanError], Vector[Token]] =
  if in.isEmpty then Either.cond(errors.isEmpty, tokens :+ Token.EOF(line), errors)
  else
    val (n, inc, errorOpt, tokenOpt) = token(in, line) match {
      case t : Token.String           => (t.lexeme.length + 2, t.lexeme.count(_ == '\n'), None, Some(t))
      case t : Token                  => (t.lexeme.length, 0, None, Some(t))
      case Skip(length, newLines)     => (length, newLines, None, None)
      case s : ScanError => (s.drop, 0, Some(s), None)
    }
    scan(in.drop(n), line + inc, errors ++ errorOpt, tokens ++ tokenOpt)

private def token(in: String, line: Int): ScanError | Skip | Token =
  import Token._
  val (head, tail) = (in.head, in.tail)
  head match
    case '(' => LeftParenthesis(line)
    case ')' => RightParenthesis(line)
    case '{' => LeftBracket(line)
    case '}' => RightBracket(line)
    case ',' => Comma(line)
    case '.' if tail.headOption.exists(isDigit) =>
      val digit = tail.takeWhile(isDigit)
      ScanError(s"Invalid numeric syntax, found '.$digit' expected '0.$digit'", line, digit.length + 1)
    case '.' => Dot(line)
    case '+' => Plus(line)
    case '-' => Minus(line)
    case ';' => Semicolon(line)
    case '*' => Star(line)
    case '!' => if tail.headOption == Some('=') then BangEqual(line) else Bang(line)
    case '=' => if tail.headOption == Some('=') then EqualEqual(line) else Equal(line)
    case '<' => if tail.headOption == Some('=') then LessThanEqual(line) else LessThan(line)
    case '>' => if tail.headOption == Some('=') then GreaterThanEqual(line) else GreaterThan(line)
    case '/' => tail.headOption match
      case Some('/') => Skip(in.takeWhile(_ != '\n').length, 0)
      case Some('*') => parseMultiLine(in, line)
      case _ => Slash(line)
    case ' ' | '\r' | '\t' => Skip(1, 0)
    case '\n' => Skip(1, 1)
    case '"' => parseString(in, line)
    case x if isDigit(x) => parseNumber(in, line)
    case x if isAlpha(x) => parseIdentifier(in, line)
    case _ => ScanError(s"Unexpected character: '$head'", line, 1)

private def parseMultiLine(in: String, line: Int): io.lox.ScanError | io.lox.Skip =
  parseMultiLine(in.drop(2), line, "/*", 1, false)
@tailrec
private def parseMultiLine(in: String, line: Int, drop: String, nestingLevel: Int, hadError: Boolean): io.lox.ScanError | io.lox.Skip =
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

private def parseString(in: String, line: Int): ScanError | Token =
  val (str, rest) = in.tail.span(_ != '"')
  if rest.headOption == Some('"') then
    Token.String(str, str, line)
  else ScanError("Unterminated string.", line, in.length)

private def isDigit(c: Char): Boolean = ('0' to '9').containsTyped(c)
private def isAlpha(c: Char): Boolean = ('a' to 'z').containsTyped(c) || ('A' to 'Z').containsTyped(c) || c == '_'
private def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

private def parseNumber(in: String, line: Int): ScanError | Token =
  val (prefix, rest) = in.span(isDigit)
  val suffix: String = if rest.headOption == Some('.') then "." + rest.tail.takeWhile(isDigit) else ""
  val lexeme = prefix + suffix
  if (suffix == ".") then
    ScanError(s"Invalid numeric syntax, found '${prefix}.' expected '${prefix}.0'", line, lexeme.length)
  else
    Token.Number(lexeme, lexeme.toDouble, line)

private def parseIdentifier(in: String, line: Int) =
  val word = in.takeWhile(isAlphaNumeric)
  Try(ReservedWords.valueOf(word))
    .toOption
    .fold(Token.Identifier(word, line)) {
      Token.ReservedWord(word, _, line)
    }

/**
 * used for consuming tokens not considered in parser
 * e.g. newlines, whitespace, comments
 * @param length to advance to next token
 * @param newLines present in the consumed contents
 */
private case class Skip(length: Int, newLines: Int)

enum Token(val lexeme: String, val line: Int):
  case LeftParenthesis(l: Int) extends Token("(", l)
  case RightParenthesis(l: Int) extends Token(")", l)
  case LeftBracket(l: Int) extends Token("{", l)
  case RightBracket(l: Int) extends Token("}", l)
  case Comma(l: Int) extends Token(",", l)
  case Dot(l: Int) extends Token(".", l)
  case Minus(l: Int) extends Token("-", l)
  case Plus(l: Int) extends Token("+", l)
  case Semicolon(l: Int) extends Token(";", l)
  case Slash(l: Int) extends Token("/", l)
  case Star(l: Int) extends Token("*", l)
  case Bang(l: Int) extends Token("!", l)
  case BangEqual(l: Int) extends Token("!=", l)
  case Equal(l: Int) extends Token("=", l)
  case EqualEqual(l: Int) extends Token("==", l)
  case LessThanEqual(l: Int) extends Token("<=", l)
  case LessThan(l: Int) extends Token("<", l)
  case GreaterThanEqual(l: Int) extends Token(">=", l)
  case GreaterThan(l: Int) extends Token(">", l)
  case String(override val lexeme: Predef.String, value: Predef.String, l: Int) extends Token(lexeme, l)
  case Number(override val lexeme: Predef.String, value: Double, l: Int) extends Token(lexeme, l)
  case ReservedWord(override val lexeme: Predef.String, `type`: ReservedWords, l: Int) extends Token(lexeme, l)
  case Identifier(override val lexeme: Predef.String, l: Int) extends Token(lexeme, l)
  case EOF(l: Int) extends Token("EOF", l)

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