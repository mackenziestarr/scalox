import scala.io.{Source, StdIn}
import io.lox.{Expression, parse, scan}

@main def lox(args: String*): Unit = args match
    case Nil => runPrompt
    case fileName :: Nil => runFile(fileName)
    case _ =>
        println("Usage: slox [script]")
        sys.exit(64)

def runFile(fileName: String): Unit =
    val source = Source.fromFile(fileName).mkString
    val res = run(source)
    if (res.isLeft) sys.exit(64)

def runPrompt: Unit =
    while (true) {
        print("> ")
        val line = StdIn.readLine
        println(line)
        if (line == null) {
            println("bye.")
            return ()
        }
        val res = run(line)
    }

def run(in: String): Either[Vector[String], Expression] =
    val parsed = for
        tokens <- scan(in)
        parsed <- parse(tokens)
    yield parsed
    println(parsed)
    parsed