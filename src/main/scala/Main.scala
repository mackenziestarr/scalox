import io.lox.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.{Source, StdIn}
import io.lox.Console

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
    case e: ParseError => sys.exit(64)
    case e: List[ScanError] => sys.exit(64)
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
    e => println(e)
  }
  runPrompt
}

type Errors = List[ScanError] | ParseError | RuntimeError

def run(in: String)(using Console): Either[Errors, Unit] =
  for
    tokens <- scan(in)
    parsed <- parse(tokens)
    result <- eval(parsed)
  yield result