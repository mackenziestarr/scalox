trait Showable:
  def show: String

sealed trait LoxError(message: String) extends Showable

object ParseError:
  def apply(message: String, token: Token): ParseError = ParseError(message, Some(token))
  val empty: ParseError = ParseError("input was empty", None)

case class ParseError(message: String, tokenOpt: Option[Token]) extends LoxError(message):
  def show = tokenOpt.fold(message) {
    token => token match
      case Token(TokenType.EOF) => s"[line ${token.line}] Error at end of file: ${message}"
      case t => s"[line ${token.line}] Error at '${token.lexeme}': ${message}"
  }

case class ScanError(message: String, line: Int, drop: Int) extends LoxError(message):
  def show: String = s"[line ${line}] Error: ${message}"

case class RuntimeError(token: Token, message: String) extends RuntimeException(message) with LoxError(message):
  def show = s"${message}\n[line ${token.line}]"

// TODO added these because of type erasure of List[ParseError] and List[ScanError], how to avoid?
case class ParseErrors(values: List[ParseError]) extends Showable:
  def show = values.map(_.show).mkString("\n")
case class ScanErrors(values: List[ScanError]) extends Showable:
  def show = values.map(_.show).mkString("\n")