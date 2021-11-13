import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.{Source, StdIn}
import scala.Console.err as StdErr

@main def lox(args: String*): Unit =
  given Console = new Console:
    override def println(s: String) = scala.Predef.println(s)
  args match
    case Nil => runPrompt
    case fileName :: Nil => runFile(fileName)
    case _ => println("Usage: slox [script]"); sys.exit(64)

def runFile(fileName: String)(using Console): Unit =
  val source = Source.fromFile(fileName).mkString
  val res = run(source, new Environment(None))
  res.left.foreach {
    case e : (ScanErrors | ParseErrors) => StdErr.println(e.show); sys.exit(65)
    case e : RuntimeError => StdErr.println(e.show); sys.exit(70)
  }

def runPrompt(using Console): Unit = runPrompt(new Environment(None))
@tailrec
def runPrompt(env: Environment)(using Console): Unit = {
  print("> ")
  val line = StdIn.readLine
  if (line == null) {
    return println("bye.")
  }
  val res = run(line, env)
  res.left.foreach {
    case e : (ScanErrors | ParseErrors) => StdErr.println(e.show)
    case e : RuntimeError => StdErr.println(e.show)
  }
  runPrompt(res.getOrElse(env))
}

type Errors = ScanErrors | ParseErrors | RuntimeError

def run(in: String, env: Environment)(using Console): Either[Errors, Environment] =
  for
    tokens <- scan(in)
    parsed <- parse(tokens)
    result <- eval(parsed, env)
  yield result