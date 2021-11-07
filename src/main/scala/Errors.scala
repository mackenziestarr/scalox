trait Showable:
  def show: String
  
sealed trait LoxError(message: String) extends Showable
case class ParseError(message: String, token: Token, tail: List[Token]) extends RuntimeException(message) with LoxError(message):
  def show = token match
    case Token.EOF(line) => s"[line ${line}] Error at end of file: ${message}"
    case t => s"[line ${t.line}] Error at '${token.lexeme}': ${message}"
case class ScanError(message: String, line: Int, drop: Int) extends LoxError(message):
  def show: String = s"[line ${line}] Error: ${message}"
case class RuntimeError(token: Token, message: String) extends RuntimeException(message) with LoxError(message):
  def show = s"${message}\n[line ${token.line}]"

case class ParseErrors(values: List[ParseError]) extends Showable:
  def show = values.map(_.show).mkString
case class ScanErrors(values: List[ScanError]) extends Showable:
  def show = values.map(_.show).mkString