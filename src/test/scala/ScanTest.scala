import Token.*
import TokenType.*
import ReservedWords.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class ScanTest
  extends AnyFlatSpec
  with EitherValues
  with Matchers:

  behavior of "ScanTest"
  it should "parse single tokens" in {
    scan("(){},.+-;*!!====<=<>=>/").value.map(_.`type`) shouldBe Vector(
      LeftParenthesis, RightParenthesis,
      LeftBracket, RightBracket,
      Comma, Dot, Plus, Minus, Semicolon, Star,
      Bang, BangEqual, EqualEqual, Equal, LessThanEqual,
      LessThan, GreaterThanEqual, GreaterThan, Slash, EOF
    )
  }
  it should "ignore whitespace, newlines, and comments" in {
    scan(" \t\r\n").value shouldBe List(Token(EOF, 2))
    scan("// some comment\n").value shouldBe List(Token(EOF, 2))
    scan("/* multi-line /* nested \n\n */ comment */").value shouldBe List(Token(EOF, 3))
    scan("/* multi-line /* nested */ unterminated comment").left.value.show shouldBe "[line 1] Error: Unterminated multi-line comment."
  }
  it should "parse Number" in {
    scan("20.04").value shouldBe List(Token(TokenType.Number("20.04"), 1), Token(EOF, 1))
    scan("20").value shouldBe List(Token(TokenType.Number("20"), 1), Token(EOF, 1))
  }
  it should "parse String" in {
    scan("\"hello, world\"").value shouldBe List(Token(TokenType.String("hello, world"), 1), Token(EOF, 1))
    scan("\"hello\nworld\"").value shouldBe List(Token(TokenType.String("hello\nworld"), 1), Token(EOF, 2))
    scan("\"hello, world").left.value.show shouldBe "[line 1] Error: Unterminated string."
  }
  it should "parse reserved words and identifiers" in {
    scan("var x = 2").value shouldBe
      List(
        Token(ReservedWord("var",`var`), 1),
        Token(Identifier("x") ,1),
        Token(Equal, 1),
        Token(TokenType.Number("2"), 1),
        Token(EOF, 1))
  }
  it should "error on unrecognized characters" in {
    scan("^").left.value.show shouldBe "[line 1] Error: Unexpected character: '^'"
  }