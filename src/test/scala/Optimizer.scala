
import yafl.SourceFile
import yafl.optimizer.Optimizer
import yafl.parser.Parser
import yafl.syntax.{Syntax, TermTree}
import yafl.typer.{TypedProgram, Typer}

final class OptimizerTests extends munit.FunSuite:

  test("Integer constant folding"):
    val optimized = optimize("1 + 2 + 3")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(6) => ()

  test("Boolean constant folding"):
    val optimized = optimize("1 > 2")
    (optimized.syntax.value : @unchecked) match
      case TermTree.BooleanLiteral(false) => ()

  test("Boolean constant folding2"):
    val optimized = optimize("true && true")
    (optimized.syntax.value : @unchecked) match
      case TermTree.BooleanLiteral(false) => ()

  test("normalization"):
    import TermTree.TermApplication as F
    val optimized = optimize("let x = 0; x + 1")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(1) => ()

  test("dead code elimination"):
    val optimized = optimize("let x = 99 ; if true then 1 else 2")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(1) => ()

  test("constant propagation and folding cascade"):
    val optimized = optimize("let x = 2 ; x + 2 * (x * x)")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(10) => ()
  
  test("inlining functions"):
    val optimized = optimize("((x: Int) => x + x) 5")
    
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(10) => ()

  /** Compiles `input` to a WebAssembly module and returns an instance of it. */
  private def optimize(input: String): TypedProgram =
    Optimizer.optimize(Typer.check(Parser.parse(SourceFile("test", input))))

end OptimizerTests
