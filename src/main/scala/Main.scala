import scala.io.{Source, StdIn}
import io.lox.{ExprResult, Expression, ParseError, RuntimeError, ScanError, eval, parse, scan, show}

@main def lox(args: String*): Unit = args match
  case Nil => runPrompt
  case fileName :: Nil => runFile(fileName)
  case _ =>
    println("Usage: slox [script]")
    sys.exit(64)

def runFile(fileName: String): Unit =
  val source = Source.fromFile(fileName).mkString
  val res = run(source)
  res.left.foreach {
    // TODO add better error handling
    case e: RuntimeError => sys.exit(70)
    case e: ParseError => sys.exit(64)
    case e: List[ScanError] => sys.exit(64)
  }

def runPrompt: Unit = loop {
  print("> ")
  val line = StdIn.readLine
  if (line == null) {
    println("bye.")
    return ()
  }
  val res = run(line)
  res.left.foreach {
    e => println(e)
  }
}

inline def loop(body: => Unit) = while (true) {
  body
}

type Errors = List[ScanError] | ParseError | RuntimeError

def run(in: String): Either[Errors, Unit] =
  for
    tokens <- scan(in)
    _ = println(tokens)
    parsed <- parse(tokens)
    _ = println(parsed)
    result <- eval(parsed)
  yield result