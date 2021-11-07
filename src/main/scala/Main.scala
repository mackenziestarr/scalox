import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.{Source, StdIn}

@main def lox(args: String*): Unit =
  given Console = new Console:
    override def println(s: String) = scala.Predef.println(s)

  args match
    case Nil => runPrompt
    case fileName :: Nil => runFile(fileName)
    case _ =>
      println("Usage: slox [script]")
      sys.exit(64)

def runFile(fileName: String)(using Console): Unit =
  val source = Source.fromFile(fileName).mkString
  val res = run(source)
  res.left.foreach {
    // TODO add better error handling
    case e: RuntimeError => sys.exit(70)
    case ScanErrors(errors) => sys.exit(64)
    case ParseErrors(errors) => sys.exit(64)
  }

@tailrec
def runPrompt(using Console): Unit = {
  print("> ")
  val line = StdIn.readLine
  if (line == null) {
    return println("bye.")
  }
  val res = run(line)
  res.left.foreach {
    // TODO add better error handling
    case e: RuntimeError => println(e)
    case ScanErrors(errors) => errors.map(_.show).foreach(println)
    case ParseErrors(errors) => errors.map(_.getMessage).foreach(println)
  }
  runPrompt
}



type Errors = ParseErrors | ScanErrors | RuntimeError

def run(in: String)(using Console): Either[Errors, Unit] =
  for
    tokens <- scan(in)
    parsed <- parse(tokens)
    result <- eval(parsed)
  yield result