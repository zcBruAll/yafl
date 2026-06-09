package yafl

import com.dylibso.chicory.tools.wasm.Wat2Wasm

import java.io.FileNotFoundException
import scala.io.AnsiColor.{BOLD, GREEN, RED, RESET}

import yafl.emitter.Emitter
import yafl.parser.Parser
import yafl.optimizer.Optimizer
import yafl.typer.Typer

/** The type of a file being generate by a compiler run. */
enum OutputContents:

  /** The abstract syntax that has been parsed. */
  case Syntax

  /** The human-readable form of the WebAssembly that has been compiled. */
  case Wat

  /** The binary form of the WebAssembly that has been compiled. */
  case Wasm

  /** Execute the WebAssembly code and return its result */
  case Execute

end OutputContents

/** The command line arguments that have been passed to start the program. */
case class Arguments(filepath: String, output: String, stage: OutputContents)

object Arguments:

  /** An error that occurred while parsing command line arguments. */
  trait Error extends Exception

  /** An error indicating that a command line argument is missing. */
  final class Missing(val name: String) extends Error:
    override def toString(): String =
      s"missing argument: ${name}"

  /** An error indicating that a command line argument was unexpected. */
  final class Unexpected(val argument: String) extends Error:
    override def toString(): String =
      s"unexpected argument: '${argument}'"

  /** Parses command line arguments from `argv`, which is the sequence of strings that has been
    * passed to the main function.
    */
  def apply(argv: Array[String]): Arguments =
    def loop(
        i: Int, filepath: Option[String], output: Option[String], stage: OutputContents
    ): Arguments =
      if i >= argv.length then filepath match
        case Some(f) => Arguments(f, output.getOrElse("out"), stage)
        case None => throw Missing("filename")
      else argv(i) match
        case "-o" if (i + 1) < argv.length =>
          loop(i + 2, filepath, Some(argv(i + 1)), stage)
        case "-o" =>
          throw Missing("output")
        case "--syntax" =>
          loop(i + 1, filepath, output, OutputContents.Syntax)
        case "--wat" =>
          loop(i + 1, filepath, output, OutputContents.Wat)
        case "--wasm" =>
          loop(i + 1, filepath, output, OutputContents.Wasm)
        case "--execute" =>
          loop(i + 1, filepath, output, OutputContents.Execute)
        case a if a.startsWith("-") =>
          throw Unexpected(a)
        case a if filepath.isEmpty =>
          loop(i + 1, Some(a), output, stage)
        case a =>
          throw Unexpected(a)
    loop(0, None, None, OutputContents.Wasm)

end Arguments

given scala.util.CommandLineParser.FromString[Arguments] with

  def fromString(s: String): Arguments =
    Arguments(s.split("\\s+"))

end given

/** The entry point of the application.
  *
  * @param filepath The path to the input file.
  */
@main def Main(argv: String*): Unit = {
  try
    // Parse the command line arguments.
    val arguments = Arguments(argv.toArray)

    // Load the source file from disk.
    val source = SourceFile.contentsOf(arguments.filepath)

    // Run the compilation pipeline.
    val untyped = Parser.parse(source)
    if arguments.stage == OutputContents.Syntax then
      write(untyped.toString, arguments.output)
    else
      val typed = Typer.check(untyped)
      val optimized = Optimizer.optimize(typed)
      val wat = Emitter.emit(optimized)
      arguments.stage match
        case OutputContents.Wat =>
          write(wat, arguments.output)
          
        case OutputContents.Wasm =>
          write(java.util.Arrays.toString(Wat2Wasm.parse(wat)), arguments.output)
          
        case OutputContents.Execute =>
          import com.dylibso.chicory.wasm.Parser as WasmParser
          import com.dylibso.chicory.runtime.Instance
          
          val wasmBytes = Wat2Wasm.parse(wat)
          val wasmModule = WasmParser.parse(wasmBytes)
          val instance = Instance.builder(wasmModule).build()
          
          val result = instance.`export`("main").apply()(0)
          println(s"Result: $result")
          
        case _ => ()

  catch
    case e: Arguments.Error => error(None, e.toString)
    case e: FileNotFoundException => error(None, s"no such file: ${e.getMessage()}")
    case e: Diagnostic => render(e)
    case e => throw e
}

/** Writes `contents`` to `destination`, which is either a file path or the string "-", in which
  * in denotes the standard output.
  */
def write(contents: String, destination: String): Unit =
  if destination == "-" then
    println(contents)
  else
    val f = new java.io.File(destination)
    val p = new java.io.PrintWriter(f)
    try { p.write(contents) } finally { p.close() }

/** Logs a success to the standard output. */
def success(message: String): Unit =
  println(s"${GREEN}${BOLD}success:${RESET} ${message}")

/** Logs an error to the standard error at `location`. */
def error(location: Option[SourceSpan], message: String): Unit =
  location match
    case Some(l) =>
      System.err.println(s"${l}: ${RED}${BOLD}error:${RESET} ${message}")
    case _ =>
      System.err.println(s"${RED}${BOLD}error:${RESET} ${message}")

/** Logs a diagnostic to the standard error. */
def render(d: Diagnostic): Unit = {
  // Log the message of the diagnostic.
  error(Some(d.span), d.description)

  // Log the line at which the diagnostic occurred.
  val source = d.span.source
  val text = source.lineContents(source.lineContaining(d.span.start) )
  System.err.println(text)

  // Log the column atwhich the diansotic occurred.
  val location = source.lineAndColumn(d.span.start)
  System.err.print(" " * (location.column - 1))
  System.err.println("^")
}
