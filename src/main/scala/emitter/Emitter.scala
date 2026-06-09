package yafl.emitter

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}
import yafl.{Diagnostic, Rope}

object Emitter:

  /** The context in which code generation is taking place.
    *
    * @param types A map from a term to its type.
    * @param locals A list of Wasm locals accumulated, paired with their types
    * @param nextId A monotonically increasing counter to ensure unique names
    * @param hoisted A list of top-level WebAssembly functions (closures)
    */
  case class Context(
    types: Map[Syntax[TermTree], Type],
    locals: List[(String, String)] = Nil,
    nextId: Int = 0,
    hoisted: List[String] = List(
      // #argv as a closure pointer
      "(func $lambda_0 (type $closure_sig) (param $env i32) (param $arg i32) (result i32) (local.get $arg) (call $#argv))"
    )
  )

  /** Generate a unique local name and update the context */
  private def uniqueLocal(name: String, wasmType: String)(using Context): Result[String] =
    val uniqueName = s"${name}_${context.nextId}"
    val nextContext = context.copy(
      locals = (uniqueName, wasmType) :: context.locals,
      nextId = context.nextId + 1
    )
    yafl.Result(uniqueName)(using nextContext)

  /** The result of generating the code of an expression. */
  type Result[+T] = yafl.Result[T, Context]

  /** Returns code of `program`. */
  def emit(program: TypedProgram): String =
    val context = Context(program.types)
    val mainResult = emitMain(program.syntax)(using context)
    val mainCode = mainResult.value
    val finalContext = mainResult.state

    val functions = finalContext.hoisted.mkString("\n")
    val tableSize = finalContext.hoisted.length
    val tableElements = if (tableSize > 0) 
      s"(elem (i32.const 0) ${(0 until tableSize).map(i => s"$$lambda_$i").mkString(" ")})" 
    else ""

    s"""(module 
       |  (memory $$__m 1) 
       |  (global $$heap_ptr (mut i32) (i32.const 1024))
       |  (type $$closure_sig (func (param i32 i32) (result i32)))
       |  (table $$tbl ${math.max(1, tableSize)} funcref)
       |  $tableElements
       |  ${argc} 
       |  ${argv} 
       |  $functions
       |  ${mainCode}
       |)""".stripMargin

  /** The code of the built-in `argc` function. */
  private val argc: String =
    """
    (func $#argc (result i32)
      (i32.const 0x0000)
      (i32.load)
    )
    """.stripIndent

  /** The code of the built-in `argv` function. */
  private val argv: String =
    """
    (func $#argv (param $i i32) (result i32)
      (i32.const 4) (local.get $i) (i32.mul)
      (i32.const 4) (i32.add)
      (i32.load)
    )
    """.stripIndent

  /** Returns the code of the main function. */
  private def emitMain(body: Syntax[TermTree])(using Context): Result[Rope] = {
    val output = context.types(body) match
      case Type.Ground.Bool | Type.Ground.Int =>
        "i32"
      case u =>
        throw Diagnostic(s"root term should have 'Int', found '${u}'", body.span)

    emitAsValue(body, Map.empty).and { code =>
      val localCode = context.locals.reverse.map {
        case (n, t) => s"(local $$${n} ${t})"
      }.mkString(" ")

      val prefix = if localCode.isEmpty then "" else localCode + " "

      result(Rope(s"(func (export \"main\") (result ${output}) ${prefix}") ++ code ++ Rope(")"))
    }
  }

  /** Recursively finds free variables to determine closure captures */
  private def freeVars(tree: TermTree): Set[String] = tree match
    case TermTree.Variable(n) => 
      if (n.startsWith("#") || n.startsWith("infix") || n.startsWith("prefix")) Set.empty 
      else Set(n)
    case TermTree.TermAbstraction(p, _, b) => freeVars(b.value) - p.value.name
    case TermTree.Binding(n, i, b) => freeVars(i.value) ++ (freeVars(b.value) - n.value.name)
    case TermTree.TermApplication(f, a) => freeVars(f.value) ++ freeVars(a.value)
    case TermTree.Conditional(c, s, f) => freeVars(c.value) ++ freeVars(s.value) ++ freeVars(f.value)
    case TermTree.TypeAbstraction(_, b) => freeVars(b.value)
    case TermTree.TypeApplication(f, _) => freeVars(f.value)
    case TermTree.RecursiveAbstraction(n, _, d) => freeVars(d.value) - n.value.name
    case _ => Set.empty

  /** Compiles a lambda to a top-level Wasm function and returns code to allocate its closure */
  private def emitClosure(
    tree: TermTree.TermAbstraction,
    recursiveName: Option[String],
    env: Map[String, String]
  )(using Context): Result[Rope] = {
    // Identify captures
    val capturesSet = freeVars(tree)
    val filteredCaptures = recursiveName.map(n => capturesSet - n).getOrElse(capturesSet)
    val captures = filteredCaptures.toList.sorted
    
    val tableIdx = context.hoisted.length
    val funcName = s"$$lambda_${tableIdx}"
    
    // Map source variables to WASM locals
    val closureEnvMap = captures.zipWithIndex.map { case (c, idx) =>
      c -> s"cap_$c"
    }.toMap + (tree.parameter.value.name -> "arg") ++ recursiveName.map(n => (n -> "env"))
    
    // Compile the body in a fresh local context
    val subContext = context.copy(locals = Nil)
    val bodyCodeResult = emitAsValue(tree.body, closureEnvMap)(using subContext)
    val bodyCode = bodyCodeResult.value
    val subContextAfter = bodyCodeResult.state
    
    // Construct the WASM function definitions
    val localsDeclarations = subContextAfter.locals.reverse.map { case (n, t) => s"(local $$${n} ${t})" }.mkString(" ")
    val loadCaptures = captures.zipWithIndex.map { case (c, idx) =>
      val offset = (idx + 1) * 4
      s"(local $$cap_$c i32) (local.set $$cap_$c (i32.load offset=$offset (local.get $$env)))"
    }.mkString(" ")
    
    val functionWasm = s"(func $funcName (type $$closure_sig) (param $$env i32) (param $$arg i32) (result i32) $localsDeclarations $loadCaptures $bodyCode)"
    
    val updatedContext = context.copy(
      nextId = subContextAfter.nextId,
      hoisted = context.hoisted :+ functionWasm
    )
    
    // Emit the code to allocate the closure struct on the heap
    uniqueLocal("closure_ptr", "i32")(using updatedContext).and { ptrName =>
      val allocCode = Rope(s"""
        (local.set $$${ptrName} (global.get $$heap_ptr))
        (i32.store (local.get $$${ptrName}) (i32.const $tableIdx))
        ${captures.zipWithIndex.map { case (c, idx) =>
            val offset = (idx + 1) * 4
            val localName = env.getOrElse(c, c)
            s"(i32.store offset=$offset (local.get $$${ptrName}) (local.get $$${localName}))"
        }.mkString(" ")}
        (global.set $$heap_ptr (i32.add (global.get $$heap_ptr) (i32.const ${4 + captures.length * 4})))
        (local.get $$${ptrName})
      """)
      result(allocCode)
    }
  }

  /** Returns the code computing the value expressed by `tree` */
  private def emitAsValue(tree: Syntax[TermTree], env: Map[String, String] = Map.empty)(using Context): Result[Rope] = {
    tree.value match
      case TermTree.Variable(n) =>
        n match
          case "#argc" => result(Rope(s"(call $$#argc)"))
          case "#argv" => 
            // Closure to #argv pointer
            uniqueLocal("argv_ptr", "i32").map { ptrName =>
              Rope(s"""
                (local.set $$${ptrName} (global.get $$heap_ptr))
                (i32.store (local.get $$${ptrName}) (i32.const 0))
                (global.set $$heap_ptr (i32.add (global.get $$heap_ptr) (i32.const 4)))
                (local.get $$${ptrName})
              """)
            }
          case _ => 
            val name = env.getOrElse(n, n)
            result(Rope(s"(local.get $$${name})"))

      case TermTree.IntegerLiteral(n) =>
        result(Rope(s"(i32.const ${n})"))

      case TermTree.BooleanLiteral(n) =>
        result(Rope(s"(i32.const ${if n then 1 else 0})"))

      case TermTree.Binding(name, initializer, body) =>
        emitAsValue(initializer, env).and { initializerCode =>
          val wasmType = context.types(initializer) match
            case Type.Ground.Bool | Type.Ground.Int => "i32"
            case _ => "i32"
          uniqueLocal(name.value.name, wasmType).and { uniqueName =>
            val newEnv = env.updated(name.value.name, uniqueName)
            emitAsValue(body, newEnv).map { bodyCode => 
              initializerCode ++ Rope(s"(local.set $$${uniqueName}) ") ++ bodyCode
            }
          }
        }

      case TermTree.TermApplication(callee, a) => callee.value match
        case TermTree.TermApplication(InfixOperator(f), b) =>
          emitAsValue(b, env).and((lhs) => emitAsValue(a, env).map { (rhs) =>
            val operation = f match
              case InfixOperator.Add => "(i32.add)"
              case InfixOperator.Sub => "(i32.sub)"
              case InfixOperator.Mul => "(i32.mul)"
              case InfixOperator.Div => "(i32.div_s)"
              case InfixOperator.Eq  => "(i32.eq)"
              case InfixOperator.Neq => "(i32.ne)"
              case InfixOperator.Lt  => "(i32.lt_s)"
              case InfixOperator.Lte => "(i32.le_s)"
              case InfixOperator.Gt  => "(i32.gt_s)"
              case InfixOperator.Gte => "(i32.ge_s)"
              case InfixOperator.And => "(i32.and)"
              case InfixOperator.Or  => "(i32.or)"
            lhs ++ rhs ++ Rope(operation)
          })

        case _ =>
          emitAsValue(callee, env).and { calleeCode =>
            uniqueLocal("closure_env", "i32").and { envName =>
              emitAsValue(a, env).map { argCode =>
                calleeCode ++
                Rope(s"(local.set $$${envName}) ") ++
                Rope(s"(local.get $$${envName}) ") ++
                argCode ++
                Rope(s"(local.get $$${envName}) ") ++
                Rope(s"(i32.load) ") ++
                Rope(s"(call_indirect (type $$closure_sig)) ")
              }
            }
          }

      case e: TermTree.TermAbstraction => 
        emitClosure(e, None, env)

      case e: TermTree.RecursiveAbstraction => 
        e.definition.value match
          case abstraction: TermTree.TermAbstraction => 
            emitClosure(abstraction, Some(e.name.value.name), env)
          case _ => 
            throw Diagnostic("fix definition must be a lambda", tree.span)

      case TermTree.TypeAbstraction(_, body) => 
        emitAsValue(body, env)
        
      case TermTree.TypeApplication(abstraction, _) => 
        emitAsValue(abstraction, env)

      case TermTree.Conditional(condition, success, failure) =>
        emitAsValue(condition, env).and { conditionCode =>
          emitAsValue(success, env).and { successCode =>
            emitAsValue(failure, env).map { failureCode =>
              conditionCode ++ Rope("(if (result i32) (then ") ++ successCode ++ Rope(") (else ") ++ failureCode ++ Rope("))")
            }
          }
        }

      case _ =>
        throw Diagnostic("unsupported term", tree.span)
  }

  /** Returns the current context. */
  private def context(using ctx: Context): Context =
    ctx

  /** Returns a result wrapping `value` together with the current context. */
  private def result[T](value: T)(using Context): Result[T] =
    yafl.Result(value)

end Emitter