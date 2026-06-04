
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
      case TermTree.BooleanLiteral(true) => ()

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

  test("common subexpression elimination basic"):
    val optimized = optimize("(#argc + 1) * (#argc + 1)")
    
    import TermTree.{Binding, TermApplication, Variable}
    
    (optimized.syntax.value : @unchecked) match
      case Binding(name, initializer, body) =>
        assertEquals(name.value.name, "cse_0")
        
        assert(initializer.value.isInstanceOf[TermApplication])
        
        (body.value : @unchecked) match
          case TermApplication(
            Syntax(TermApplication(Syntax(Variable("infix*"), _), Syntax(Variable("cse_0"), _)), _), 
            Syntax(Variable("cse_0"), _)
          ) => ()

  test("common subexpression elimination scoping"):
    val optimized = optimize("(x: Int) => (x * 2) + (x * 2)")
    
    import TermTree.{TermAbstraction, Binding}
    
    (optimized.syntax.value : @unchecked) match
      case TermAbstraction(param, ascription, body) =>
        assertEquals(param.value.name, "x")
        
        (body.value : @unchecked) match
          case Binding(name, initializer, innerBody) =>
            assertEquals(name.value.name, "cse_0")

  test("common subexpression elimination with constant folding"):
    val optimized = optimize("(#argc * (1 + 2)) - (#argc * (1 + 2))")
    
    import TermTree.{Binding, TermApplication, Variable, IntegerLiteral}
    
    (optimized.syntax.value : @unchecked) match
      case Binding(name, initializer, body) =>
        assertEquals(name.value.name, "cse_0")
        
        (initializer.value : @unchecked) match
          case TermApplication(Syntax(TermApplication(Syntax(Variable("infix*"), _), _), _), Syntax(IntegerLiteral(3), _)) => ()

  test("monomorphization basic application"):
    val optimized = optimize("let id = [T] => (x: T) => x ; id [Int] 42")
    
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(42) => ()

  test("monomorphization with multiple types"):
    val optimized = optimize("let id = [T] => (x: T) => x ; id [Bool] (1 < (id [Int] 2))")
    
    (optimized.syntax.value : @unchecked) match
      case TermTree.BooleanLiteral(true) => ()

  test("monomorphization with arrow types"):
    val program = """
      let applyTwice = [T] => (f: T -> T) => (x: T) => f (f x) ;
      let addOne = (n: Int) => n + 1 ;
      applyTwice [Int] addOne 10
    """
    
    val optimized = optimize(program)
    
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(12) => ()
  
  test("monomorphization preserves AST shape when not fully foldable"):
    val optimized = optimize("let id = [T] => (x: T) => x ; id [Int] #argc")
    
    import TermTree.{Variable, Binding}
    
    (optimized.syntax.value : @unchecked) match
      case v => 
        assert(!v.toString.contains("TypeApplication"))
        assert(!v.toString.contains("TypeAbstraction"))

  test("loop unrolling constant evaluation"):
    val program = """
      let f = fix loop : Int -> Int =
        (x: Int) => if x > 10 then x else loop (x * x) ;
      f 4
    """
    val optimized = optimize(program)
    
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(16) => ()

  test("loop unrolling symbolic expansion"):
    val program = """
      let f = fix loop : Int -> Int =
        (x: Int) => if x > 10 then x else loop (x * x) ;
      f #argc
    """
    val optimized = optimize(program)
    
    import TermTree.{Conditional, Binding}
    (optimized.syntax.value : @unchecked) match
      case Conditional(_, _, Syntax(Binding(_, _, Syntax(Conditional(_, _, _), _)), _)) => ()

  /** Compiles `input` to a WebAssembly module and returns an instance of it. */
  private def optimize(input: String): TypedProgram =
    Optimizer.optimize(Typer.check(Parser.parse(SourceFile("test", input))))

end OptimizerTests
