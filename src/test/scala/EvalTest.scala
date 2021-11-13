import run as runLox
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class EvalTest
  extends AnyFlatSpec
    with EitherValues
    with Matchers:

  trait Gettable:
    def get: List[String]

  it should "store and evaluate variables" in {

    val console = new Console with Gettable:
      private var list = List.empty[String]
      def get = list.reverse
      def println(s: String) = list = s :: list

    given Console = console

    val program =
      """
        | var a = 414;
        | var b = 141;
        | b = 142;
        | print a + b;
        | print a - b;
        | print a = 4;
        |""".stripMargin
    val actual: Either[Errors, Environment] = runLox(program, new Environment(None))
    // TODO figure out how to report error for Left(_)
    actual.isRight shouldBe true
    console.get shouldBe List("556", "272", "4")
  }
