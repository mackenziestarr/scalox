import scala.io.{Source, StdIn}
import io.lox.{scan, Error}

@main def lox(args: String*): Unit = {
    args match
        case Nil => runPrompt
        case fileName :: Nil => runFile(fileName)
        case _ =>
            println("Usage: slox [script]")
            sys.exit(64)
}

def runFile(fileName: String): Unit = {
    val source = Source.fromFile(fileName).mkString
    run(source)
    if (Error.occurred) sys.exit(64)
}

def runPrompt: Unit = {
    while (true) {
        print("> ")
        val line = StdIn.readLine
        println(line)
        if (line == null) {
            println("bye.")
            return ()
        }
        run(line)
        Error.occurred = false
    }
}

def run(in: String): Unit = {
    println(scan(in))
}