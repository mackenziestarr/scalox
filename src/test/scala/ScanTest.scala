import io.lox.Token.EOF
import io.lox.Token
import io.lox.Token.*
import io.lox.ReservedWords.*
import io.lox.scan
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class ScanTest
  extends AnyFlatSpec
  with EitherValues
  with Matchers:

  behavior of "ScanTest"
  it should "parse single tokens" in {
    scan("(){},.+-;*!!====<=<>=>/").value shouldBe Vector(
      LeftParenthesis(1), RightParenthesis(1), LeftBracket(1), RightBracket(1),
      Comma(1), Dot(1), Plus(1), Minus(1), Semicolon(1), Star(1),
      Bang(1), BangEqual(1), EqualEqual(1), Equal(1), LessThanEqual(1),
      LessThan(1), GreaterThanEqual(1), GreaterThan(1), Slash(1), EOF(1)
    )
  }
  it should "ignore whitespace, newlines, and comments" in {
    scan(" \t\r\n").value shouldBe Vector(EOF(2))
    scan("// some comment\n").value shouldBe Vector(EOF(2))
    scan("/* multi-line /* nested \n\n */ comment */").value shouldBe Vector(EOF(3))
    scan("/* multi-line /* nested */ unterminated comment").left.value.map(_.show) shouldBe Vector("[line 1] Error: Unterminated multi-line comment.")
  }
  it should "parse Number" in {
    scan("20.04").value shouldBe Vector(Token.Number("20.04", 20.04d, 1), EOF(1))
    scan("20").value shouldBe Vector(Token.Number("20", 20d, 1), EOF(1))
    scan("20.").left.value.map(_.show) shouldBe Vector("[line 1] Error: Invalid numeric syntax, found '20.' expected '20.0'")
    scan(".20").left.value.map(_.show) shouldBe Vector("[line 1] Error: Invalid numeric syntax, found '.20' expected '0.20'")
  }
  it should "parse String" in {
    scan("\"hello, world\"").value shouldBe Vector(Token.String("hello, world", "hello, world", 1), EOF(1))
    scan("\"hello\nworld\"").value shouldBe Vector(Token.String("hello\nworld", "hello\nworld", 1), EOF(2))
    scan("\"hello, world").left.value.map(_.show) shouldBe Vector("[line 1] Error: Unterminated string.")
  }
  it should "parse reserved words and identifiers" in {
    scan("var x = 2").value shouldBe
      Vector(ReservedWord("var",`var`,1), Identifier("x",1), Equal(1), Number("2",2.0,1), EOF(1))
  }
  it should "error on unrecognized characters" in {
    scan("^").left.value.map(_.show) shouldBe Vector("[line 1] Error: Unexpected character: '^'")
  }