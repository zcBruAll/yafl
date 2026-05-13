# Instructions

Your project consists of completing the implementation of Yafl provided in this repository.
More specifically, you should modify and/or extend the current implementation so that it can pass all tests (i.e., those executed with `sbt test`).

Yafl's compiler comprises four main stages: parser, typer, optimizer, emitter.
The typing stage has been fully implemented; your work will focus on the three others.
Most tasks related to the parser are considered *required* and must be done to complete your project.
Other tasks are considered *optional* and can be skipped if necessary.
Some optional tasks are also marked *hard* or *brutal* and are significantly more difficult to complete than others.

A primary objective of this project is to train your ability to work within a purely functional paradigm.
Hence, you are not allowed to use variables or mutable data structures to complete your tasks.
You are also not allowed to import any additional dependencies.

## Evaluation

Your work will be evaluated according to both functionality (i.e., the features that your compiler implements) and quality (i.e., the legibility and maintainability of your code).
The result of this evaluation will lead to either a bonus of at most 1 point or a malus of at most -1 point on your grade for the course.

Failure to complete required tasks will push in the negative direction, whereas the completion of optional tasks will push in the positive direction.
You must complete **all required tasks** and at least **six optional tasks** to obtain the full bonus.
Completing an advanced or brutal task counts as much as completing two or three ordinary tasks, respectively

### About tests and correctness

This project can be considered an example of [test-driven development](https://en.wikipedia.org/wiki/Test-driven_development).
You have been given a test suite describing the requirements of some specification, and your objective is to provide an implementation.
This setup is particularly convenient for modern code generation tools, as a coding assistant can often often iteratively refines an implementation until it passes all tests.

Keep in mind that a test is not a correctness proof, as your code may be used in ways that are not captured by the existing test suite.
That is especially true for compilers, for which the domains of valid inputs are particularly large and complex.
As a result, relying on just a few tests is not a good indicator of correctness.

To mitigate this issue, you can write additional tests exercising various situations, making sure that your test suite also checks for invalid inputs.
It is also recommended to add a test every time you find a bug to avoid regression in subsequent changes.

### About code quality

Code quality is a subjective property that usually cannot be measured with hard evidence.
Nonetheless, there are a few rules that can help:

1. **Document all definitions!**

   All classes and all functions *must* be documented.
   Exceptions are vanishingly rare.

   Your comment should *add* information that is not simply already in the name of your definition.
   For example, the following is a performative comment that does not make the code clearer:

   ```
   /** A token. */
   class Token
   ```

   Better documentation would (briefly) explain what a token is, as though it were a short description in a lexicon.

2. **Avoid abbreviations.**

   Use full words in your code.
   For example, `TypeApplication` is clearer than `TApp`.
   Abbreviations invite ambiguity and/or increase cognitive overload.

3. **Name things after their role.**

   Use names as an opportunity to clarify the role of an entity.
   For example, `input` is a better name than `sourceFile` to name the input file being compiled.

   If the role of a variable is obvious from the context, do not hesitate to use a single letter name, as concision reduces cognitive overload.

4. **Do not repeat yourself.**

   Avoid boilerplate as it increases cognitive overload and impedes maintainability.
   Whenever you write the same code twice, take a step back and think whether there might be an opportunity to create a meaningful abstraction.

5. **Observe the boy/girl scout rule**: leave the code cleaner than you found it.

   When you modify someone else's code, make sure to preserve or improve code quality by applying all the rules above to the code you are modifying.

### About the use of LLMs

You may use LLM‑powered tools (e.g., Claude, Copilot, etc.) to assist you with your tasks, **provided that**:

1. you fully understand and take complete responsibility for any code or text that you include in your submission; and
2. you clearly disclose the use of any assistant wherever appropriate (e.g., as comments next to generated source
code or as footnotes after generated documentation).

## Parsing Tasks

The `README.md` file of this repository documents the full grammar of Yafl, expressed in EBNF.
The current implementation contains a lexer capable of recognizing the terminal symbols of this grammar, along with an incomplete parser capable of recognizing a subset of Yafl.
Your objective is to complete this implementation.

Below are the tasks related to the parser.
It is recommended to implement them in order.
All tasks have an associated test that you can use to exercise your implementation.

> **Tip**: the entry point of the parser is the method `parse`, which parses a term.
> Since other methods are private, there is no direct means to test the parsing of a type expression.
> One simple way to work around this issue is to parse a term involving a type expression, such as a type application or a type abstraction.

- [x] **Conditionals and Bindings** (required)

  Conditionals and bindings are simple terms that are relatively easy to parse.
  Each construction is introduced with its own dedicated keyword (i.e., `if` and `let`, respectively) and has a fixed structure.
  Consequently, it is straightforward for the parser to recognize the start of a construction.
  You can take inspiration from the part of the parser that is handling the expression of a lambda to write your code.

- [x] **Type abstractions** (required)

  Type abstractions have the form `[T] => e`.
  Similarly to conditionals and bindings, they also start with a token (i.e., `[`) that cannot occur at the beginning of any other construction.
  Consequently, it is again straightforward for the parser to recognize the start of a type abstraction.

  Although the grammar specifies that more than one type parameter may be supplied, you can implement this step assuming that all type abstractions have exactly one parameter.

- [x] **Prefix terms** (required)

  Prefix terms have the form `f e`, where `f` is an operator, meaning that the occurrence of an operator at the start of a term signals the presence of a prefix term.
  In other words, the parser can apply a similar strategy as the one used for simple terms, with two caveats.
  First, *any* operator can be recognized at the start rather than one specific token.
  Second, a prefix term is not considered a simple term because it cannot occur at the right-hand side of a term application, lest the expression `x + y` would become ambiguous.

- [x] **Universal types** (required)

  Universal types (aka *forall*) have almost the same form as type abstractions but can only occur in type positions.
  In other words, the occurrence of an opening left brace can be interpreted as either a type abstraction or a universal type depending on the production rule being applied.

  Like for type abstractions, although the grammar specifies that a universal type may be introduced with more than one type variable, you can implement this step assuming that there is exactly one.

- [x] **Arrow types** (required)

  Arrow types have the form `T -> U`.
  Consequently, unlike the constructions mentioned above, the parser cannot simply use a single token to recognize the start of an arrow.
  However, notice that the occurrence of an arrow operator (i.e., `->`) following a type expression signals the presence of an arrow.
  Further, since the operator is right-associative, the parser can simply recurse to recognize the type expression on the right-hand side.

- [x] **Parenthesized types** (required)

  Just like term expressions, type expressions can be written in parentheses to override default precedence or simply to improve legibility.
  For example, the type expression `T -> U -> V` does not denote the same type as `(T -> U) -> V`.
  One describes functions from `T` to `U -> V`, the other describes functions from `T -> U` to `V`.
  Fortunately, since no other type construction involves parentheses, the occurrence of an opening parenthesis at the start of a type expression signals the presence of a parenthesized type.

- [x] **Type applications** (required)

  Type applications have the form `e [T]`.
  Similarly to arrow types, the occurrence of a left bracket following a term signals the presence of a type application.

  Although the grammar specifies that more than one type argument may be supplied, you can implement this step assuming that all type applications have exactly one argument.

- [x] **Recursive abstractions** (required)

  Recursive type abstractions have the form `fix x : T = f` where `x` is an identifier, `T` a type, and `f` an arbitrary term.
  Since the construction starts with a dedicated token, it can be recognized in the same way as other simple terms like bindings and conditionals.

  Note that the tests for recursive type abstractions rely on your parser's ability to recognize arrow types.
  In other words, these tests will fail unless your parser can properly recognize a term expression of the form `T -> U`.
  You can still write your own tests using simpler types, though.

- [ ] **Multiple parameters and arguments** (optional)

  According to the grammar, several production rules can recognize comma-separated sequences of certain constructions.
  For example, a term abstraction can be written with more than one parameter (e.g., `(x : Int, y: Int) => x + y`).

  Looking at the definitions of abstract syntax trees, however, one may be surprised to see that term abstractions have exactly one parameter.
  That is because a lambda taking more parameters can be *desugared* into its curried form during parsing.
  In other words, `(x : Int, y : Int) => x + y` can be parsed as though it had been written `(x : Int) => (y : Int) => x + y` instead.

  Likewise, type abstractions, type applications, and universal types can use a similar desugaring.
  For example, `e [T, U]` can be parsed as though it had been written `e [T] [U]`.

  Taking inspiration from the parsing of term abstractions, your task is to implement such desugaring for type abstractions, type applications, and universal types.

### About the structure of the implementation

Note that the parser is not (and does not have to) perfectly mirror the structure of the grammar.
For example, the grammar describes term applications with the following rule:

```
term-application ::=
  | type-application type-application?
```

However, the parser handles term applications in the method `lambdaOrParenthesized`, which also recognizes parenthesized terms.
This particular arrangement is done to avoid backtracking by avoiding committing too early.
The takeaway is that you should also apply your best judgment to deviate from the structure of the grammar in your parser whenever appropriate.

## Optimization Tasks

The optimizer runs on typed programs by rewriting particular patterns found in the syntax trees.
For example, [constant folding](https://en.wikipedia.org/wiki/Constant_folding) is a common optimization that consists of replacing constant expressions (e.g., `3 + 1`) by their values at compile-time (e.g., `4`).
Note that an optimization does not necessarily result in a shorter term, only a term that is (at least in principle) faster to evaluate at run-time.

Incidentally, an optimization can also result in terms that are simpler to compile.
For example, `((x : Int) => x + x) y` could be rewritten as simply `y + y` , which is not only faster to execute (one less function call) but also no longer involves an anonymous function, which would be rather difficult to compile.

Also note that applying a particular optimization may enable another optimization.
For example, applying constant propagation (see below) before constant folding makes it possible for the compiler to replace `let x = 2 ; x + 2 * (x * x)` with just `10`.
Hence, it may be necessary to apply optimizations until some fixed point is reached to obtain the best possible result.

Below are the tasks related to the optimizer.
These can be implemented in any order, but note that normalization may help dramatically reduce the number of cases to consider in other optimizations.
Most available tests may require the application of more than one optimization to pass.
Further, all tests rely on the parser behaving correctly.

- [ ] **Normalization** (optional)

  Equivalent programs can come in many shapes.
  For example, `1 + x + 2` computes the same value as `1 + 2 + x`.
  However, it is convenient for a compiler if equivalent terms have equivalent syntax.
  While this property cannot be guaranteed in general, there are a few simple transformations one can apply to *normalize* the shape of a term.

  For this task, your goal is to transform programs so that:

    1. Constants are on the left-most side of syntax trees.

       For example, the normal form of `1 + x + 2` is `1 + 2 + x`.
       In the rewritten form, `x` is on the right leaf of the tree, whereas `2` is on the left one.

    2. Bindings are moved as close as possible to the root.

       For example, the normal form of `2 + (let x = 1 ; x)` is `let x = 1 ; 2 + x`.
       In the rewritten form, `x` has moved closer to the root.

  Naturally, your normalization should not modify the semantics of the program.
  In particular, a binding cannot be moved above the binding introducing the variables occurring free in its initializer.
  For example, `(z : Int) => let x = z : x + x` is already in normal form because moving the binding outside of the term abstraction would leave `z` unbound.


- [ ] **Dead code elimination** (optional)

  [Dead code elimination](https://en.wikipedia.org/wiki/Dead-code_elimination) consists of removing unreachable code from the program.
  For example, both `if true then 1 else 2 * 3` and `let x = 2 ; a` can be rewritten `1`.

  For this task, your goal is to eliminate conditionals whose condition is provably `true` or `false` as well as bindings that have no use.
  The examples above illustrate.

- [ ] **Constant propagation** (optional)

  Constant propagation consists of replacing variables denoting a constant by their values.
  For example, `let x = 2 ; x + y` can be rewritten `2 + y`.

- [ ] **Inlining** (optional)

  Inlining consists of applying functions at compile-time.
  More formally, it is done by performing beta-reduction on term applications having a term abstraction for their callee.
  For example, `((x : Int) => x + x) y` can be rewritten `y + y`.

- [ ] **Common subexpression elimination** (optional, hard)

  [Common subexpression elimination](https://en.wikipedia.org/wiki/Common_subexpression_elimination) consists of replacing multiple occurrences of the same computation by a binding whose value is computed only once.
  For example, `(1 + x) * (1 + x)` can be rewritten `let y = 1 + x ; y + y`.

- [ ] **Loop unrolling** (optional, brutal)

  Loop unrolling consists of extracting work from a loop to eliminate control tests.
  For example, consider the following program:

  ```yafl
  let f = fix loop : Int -> Int =
    (x: Int) => if x > 10 then x else loop (x * x) ;
  f 4
  ```

  By performing loop unrolling twice, this program could be rewritten as follows:

  ```yafl
  let f = fix loop : Int -> Int =
    (x: Int) => if x > 10 then x else loop (x * x) ;
  if 4 > 10 then 4 else if (4 * 4) > 10 then (4 * 4) else f (4 * (4 * 4)) ;
  ```

  Note that the call to the recursive function `f` will disappear if we perform constant folding and dead code elimination at this point, resulting in simply `16`.

## Code Generation Tasks

The emitter runs on optimized programs and generates WebAssembly code in text format ([WAT](https://webassembly.github.io/spec/core/text/index.html)).
This output is then fed to another compiler, [Wat2Wasm](https://github.com/webassembly/wabt), which produces a binary module.

Below are the tasks related to the optimizer.
These can be implemented in any order, although it is recommended to start with normalization.
Note that all available tests rely on the parser behaving correctly.
Most tests can only pass if you have also implemented part of the optimizer or if your compiler can handle closures.

- [ ] **Built-in arithmetic and comparison** (optional)

  The current implementation can only handle two built-in operations, namely addition and subtraction.
  For this task, your goal is to support the rest.

- [ ] **Bindings** (optional)

  The current implementation does not handle local bindings, which can be implemented using local variables in WebAssembly.
  For example, the expression `let x = 2 ; x + x` can be compiled to the following:

  ```wat
  (local $x i32)
  (local.set $x (i32.const 2))
  (local.get $x)
  (local.get $x)
  (i32.add)
  ```

  Note that local bindings must be declared at the root of a function.
  Consequently, one must ensure that all local bindings in a function are declared upfront and get a different name.
  For example, consider the following program, which is legal in Yafl:

  ```yafl
  let x = 1 ;
  if x < 2 then let x = 2 ; x else x
  ```

  Once compiled in WebAssembly, the two bindings must be declared at the root of the function and be given different names.

- [ ] **Monomorphiation** (optional, hard)

  Yafl is an implementation of [System F](https://en.wikipedia.org/wiki/System_F), meaning that it supports parametric polymorphism.
  For example, the following program shows an application of the polymorphic identity:

  ```yafl
  let id = [T] => (x: T) => x ;
  id [Bool] (1 < (id [Int] 2))
  ```

  Low-level programming languages like WebAssembly typically do not feature parametric polymorphism.
  Consequently, one needs a mechanism to represent generic definitions with monomorphic constructions.

  One simple strategy is [monomorphization](https://en.wikipedia.org/wiki/Monomorphization), which consists of rewriting a copy in which generic type parameters have been replaced with their arguments.
  For example, the above program can be rewritten as follows:

  ```yafl
  ((x: Bool) => x) (1 < (((x: Int) => x) 2))
  ```

  For this task, your goal is to implement monomorphization so that your compiler can process programs involving generic definitions.
  Your implementation does not have to support non-recursive definitions, thereby ensuring that monomorphization is always applicable.

- [ ] **Closures** (optional, brutal)

  The current implementation does not handle first-class functions.
  For example, the following program cannot compile without inlining `((x : Int) => x) 1`.

  First-class functions are difficult to implement in a low-level programming language like WebAssembly because they require a mechanism to keep track of the function's environment.
  To illustrate, consider the following program

  ```yafl
  let plus_some = (i : Int) => (n : Int) => n + i ;
  let plus_four = plus_some 4 ;
  plus_four 8
  ```

  The value assigned to `plus_four` is a function having captured the argument passed to `i`.
  In a low-level programming language, the typical strategy to represent such a value is to wrap this capture together with a pointer to a function that will take it as a parameter.
  For example, one could write the following in C:

  ```c
  int plus_some(int i, int n) { return n + i; }

  typedef struct {
    int (*f)(int, int);
    int i;
  } closure;

  int main() {
    closure plus_four = { &plus_some, 4 };
    return plus_four.f(plus_four.i, 8);
  }
  ```

  Implementing this strategy involves three main steps.

  1. Detect what values are part of the function's closure.
  2. Store the these values on the heap.
  3. Create a function that accepts the closure as an extra parameter.

  There are different ways to allocate memory dynamically in WebAssembly.
  The simplest approach is to leverage WasmGC, which extends the core language with a garbage collector.

## Submission

Follow these instructions to submit your work:

1. Create a tag of your repository.
2. Write a small report with a link to this tag and, for each task you have completed:

   - the names of the persons who worked on the task
   - references to the relevant part of your implementation

3. Submit this report on [isc.hevs.ch/learn](https://isc.hevs.ch/learn/) no later than Tuesday, 26th May 23:59 (CEST).
