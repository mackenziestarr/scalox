import Expression.*
import Token.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class ParseTest
  extends AnyFlatSpec
  with EitherValues
  with Matchers:

  behavior of "Parser"
  it should "parse multiple" in {
    val actual = for {
      tokens <- scan("1 == 2 == (3 == 4);")
      parsed <- parse(tokens)
    } yield parsed
    actual.value shouldBe
      Seq(
        Statement.Expr(
          Binary(
          Binary(
            Literal(1.0d),
            EqualEqual(1),
            Literal(2.0d)
          ),
          EqualEqual(1),
          Grouping(
            Binary(
              Literal(3.0d),
              EqualEqual(1),
              Literal(4.0d))))
        )
      )
  }
  it should "error on invalid start of expression" in {
    val actual = for {
      tokens <- scan("<= 1")
      parsed <- parse(tokens)
    } yield parsed
    actual.left.value.show shouldBe ParseErrors(List(ParseError("Expected expression", Token.LessThanEqual(1), List(EOF(1))))).show
  }
  it should "parse variables" in {
    val actual = for {
      tokens <- scan("var fruit = \"apple\";\n var vegetable;\n fruit;")
      parsed <- parse(tokens)
    } yield parsed
    actual.value shouldBe Seq(
      Statement.Var(Identifier("fruit", 1), Some(Literal("apple"))),
      Statement.Var(Identifier("vegetable", 2), None),
      Statement.Expr(Var(Identifier("fruit", 3)))
    )
  }
  it should "parse complex" in {
    val actual = for {
      tokens <- scan("- 1 * 2 + 3 < 4 == 5 <= 6 - 7 / 8;")
      parsed <- parse(tokens)
    } yield parsed
    actual.value shouldBe
      Seq(Statement.Expr(
        Binary(
        Binary(
          Binary(
            Binary(
              Unary(
                Minus(1),
                Literal(1.0d)
              ),
              Star(1),
              Literal(2.0d),
            ),
            Plus(1),
            Literal(3.0d)
          ),
          LessThan(1),
          Literal(4.0d)
        ),
        EqualEqual(1),
        Binary(
          Literal(5.0d),
          LessThanEqual(1),
          Binary(
            Literal(6.0d),
            Minus(1),
            Binary(
              Literal(7.0d),
              Slash(1),
              Literal(8.0d)))))))
  }