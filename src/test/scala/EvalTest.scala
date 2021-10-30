import io.lox.InMemoryPrinter
import run as runLox
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class EvalTest
  extends AnyFlatSpec
    with EitherValues
    with Matchers:

  it should "store and evaluate variables" in {
    val printer = new InMemoryPrinter
    val program =
      """
        | var a = 414;
        | var b = 141;
        | print a + b;
        | print a - b;
        |""".stripMargin
    val actual: Either[Errors, Unit] = runLox(printer, program)
    actual.isRight shouldBe true
    printer.get shouldBe List("555", "273")
  }
