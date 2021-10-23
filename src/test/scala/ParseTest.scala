import io.lox.{scan, parse}
import io.lox.Expression._
import io.lox.Token._
import io.lox.Token
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class ParseTest
  extends AnyFlatSpec
  with EitherValues
  with Matchers:

  behavior of "Parser"
  /*it should "parse simple" in {
    val actual = for {
      tokens <- scan("2 == 3")
      parsed <- parse(tokens)
    } yield parsed
    actual.value shouldBe Binary(Literal(Token.Number("2", 2.0d, 1)), Token.EqualEqual(1), Literal(Token.Number("3", 3.0d, 1)))
  }*/
  it should "parse mutliple" in {
    val actual = for {
      tokens <- scan("1 == 2 == 3")
      parsed <- parse(tokens)
    } yield parsed
    actual.value shouldBe
      Binary(
        Binary(
          Literal(Number("1",1.0,1)),
          EqualEqual(1),
          Literal(Number("2",2.0,1))
        ),
        EqualEqual(1),
        Literal(Number("3",3.0,1)))
  }
  it should "parse complex" in {
    val actual = for {
      tokens <- scan("- 1 * 2 + 3 < 4 == 5 <= 6 - 7 / 8")
      parsed <- parse(tokens)
    } yield parsed
    actual.value shouldBe
      Binary(
        Binary(
          Binary(
            Binary(
              Unary(
                Minus(1),
                Literal(Token.Number("1", 1.0d, 1))
              ),
              Token.Star(1),
              Literal(Token.Number("2", 2.0d, 1)),
            ),
            Token.Plus(1),
            Literal(Token.Number("3", 3.0d, 1))
          ),
          Token.LessThan(1),
          Literal(Token.Number("4", 4.0d, 1))
        ),
        Token.EqualEqual(1),
        Binary(
          Literal(Token.Number("5", 5.0d, 1)),
          Token.LessThanEqual(1),
          Binary(
            Literal(Token.Number("6", 6.0d, 1)),
            Token.Minus(1),
            Binary(
              Literal(Token.Number("7", 7.0d, 1)),
              Token.Slash(1),
              Literal(Token.Number("8", 8.0d, 1))))))
  }