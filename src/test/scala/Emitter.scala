import com.dylibso.chicory
import com.dylibso.chicory.tools.wasm.Wat2Wasm

import java.io.File

import yafl.SourceFile
import yafl.emitter.Emitter
import yafl.optimizer.Optimizer
import yafl.parser.Parser
import yafl.typer.Typer

final class EmitterTests extends munit.FunSuite:

  test("argc"):
    val input = SourceFile("test", "#argc")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    writeArguments(wasm, IArray(31, 11))
    assertEquals(main.apply()(0), 2L)

  test("argv"):
    val input = SourceFile("test", "(#argv 0) + (#argv 1)")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    writeArguments(wasm, IArray(40, 2))
    assertEquals(main.apply()(0), 42L)

  test("integer addition"):
    val input = SourceFile("test", "40 + 2")
    val main = compile(input).`export`("main")
    assertEquals(main.apply()(0), 42L)

  test("local binding"):
    // #argc to prevent the optimizer to fold the code
    val input = SourceFile("test", "let x = #argc ; x + x")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    
    writeArguments(wasm, IArray(10, 20, 30)) 
    
    assertEquals(main.apply()(0), 6L)

  test("local binding shadowing"):
    val input = SourceFile("test", "let x = #argc ; let x = #argc + 1 ; x + x")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    
    writeArguments(wasm, IArray(99, 99))

    assertEquals(main.apply()(0), 6L)

  test("comparison operators"):
    val input = SourceFile("test", "#argc < 5")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    
    writeArguments(wasm, IArray(10, 20, 30))
    assertEquals(main.apply()(0), 1L)

  test("basic closure"):
    val input = SourceFile("test", """
      let plus_some = (i : Int) => (n : Int) => n + i ;
      let plus_four = plus_some 4 ;
      plus_four 8
    """)
    val wasm = compile(input)
    val main = wasm.`export`("main")
    assertEquals(main.apply()(0), 12L)

  test("multiple captures"):
    val input = SourceFile("test", """
      let a = 10 ;
      let b = 20 ;
      let f = (x: Int) => a + b + x ;
      f 5
    """)
    val wasm = compile(input)
    val main = wasm.`export`("main")
    assertEquals(main.apply()(0), 35L)

  test("nested closures"):
    val input = SourceFile("test", """
      let f = (a: Int) => (b: Int) => (c: Int) => a + b + c ;
      let fa = f 1 ;
      let fb = fa 2 ;
      fb 3
    """)
    val wasm = compile(input)
    val main = wasm.`export`("main")
    assertEquals(main.apply()(0), 6L)

  test("higher-order function"):
    val input = SourceFile("test", """
      let apply = (f: Int -> Int) => (x: Int) => f x ;
      let add_five = (y: Int) => y + 5 ;
      let apply_add_five = apply add_five ;
      apply_add_five 10
    """)
    val wasm = compile(input)
    val main = wasm.`export`("main")
    assertEquals(main.apply()(0), 15L)

  test("#argv as a first-class function"):
    val input = SourceFile("test", """
      let get_arg = #argv ;
      (get_arg 0) + (get_arg 1)
    """)
    val wasm = compile(input)
    val main = wasm.`export`("main")
    writeArguments(wasm, IArray(100, 50))
    assertEquals(main.apply()(0), 150L)
    
  test("recursive closure"):
    val input = SourceFile("test", """
      let factorial = fix loop : Int -> Int =
        (n : Int) =>
          if n < 2 then 1 else n * (loop (n - 1)) ;
      factorial 5
    """)
    val wasm = compile(input)
    val main = wasm.`export`("main")
    assertEquals(main.apply()(0), 120L)

  /** Compiles `input` to a WebAssembly module and returns an instance of it. */
  private def compile(input: SourceFile): chicory.runtime.Instance =
    val program =  Optimizer.optimize(Typer.check(Parser.parse(input)))
    val binary = Wat2Wasm.parse(Emitter.emit(program))
    val m = chicory.wasm.Parser.parse(binary)
    chicory.runtime.Instance.builder(m).build()

  /** Initializes the command-line arguments of `wasm` to `values`. */
  private def writeArguments(wasm: chicory.runtime.Instance, values: IArray[Int]): Unit =
    val m = wasm.memory()
    m.writeI32(0, values.length)
    for i <- 0 until values.length do m.writeI32(4 + (i * 4), values(i))

end EmitterTests
