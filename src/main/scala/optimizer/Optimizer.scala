package yafl.optimizer

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}
import yafl.syntax.TermTree.TermAbstraction
import yafl.syntax.TermTree.BooleanLiteral

object Optimizer:

  /** Returns `program` optimized. */
  def optimize(program: TypedProgram): TypedProgram =
    val (optimized, updated) = rewrite(program.syntax, program.types)
    TypedProgram(optimized, updated)

  /** Recursively traverses and optimizes the syntax tree from the bottom up */
  private def rewrite(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): (Syntax[TermTree], TypedProgram.TypeAssignments) = {
    val (optimizedChild, updatedTypes) = tree.value match
      case e: TermTree.TermApplication =>
        val (f, ts1) = rewrite(e.abstraction, types)
        val (a, ts2) = rewrite(e.argument, ts1)
        (Syntax(TermTree.TermApplication(f, a), tree.span), ts2)
      case e: TermTree.Conditional =>
        val (c, ts1) = rewrite(e.condition, types)
        val (s, ts2) = rewrite(e.success, ts1)
        val (f, ts3) = rewrite(e.failure, ts2)
        (Syntax(TermTree.Conditional(c, s, f), tree.span), ts3)
      case e: TermTree.Binding =>
        val (i, ts1) = rewrite(e.initializer, types)
        val (b, ts2) = rewrite(e.body, ts1)
        (Syntax(TermTree.Binding(e.name, i, b), tree.span), ts2)
      case e: TermTree.TermAbstraction =>
        val (b, ts) = rewrite(e.body, types)
        (Syntax(TermTree.TermAbstraction(e.parameter, e.ascription, b), tree.span), ts)
      case e: TermTree.TypeAbstraction =>
        val (b, ts) = rewrite(e.body, types)
        (Syntax(TermTree.TypeAbstraction(e.parameter, b), tree.span), ts)
      case e: TermTree.TypeApplication =>
        val (a, ts) = rewrite(e.abstraction, types)
        (Syntax(TermTree.TypeApplication(a, e.argument), tree.span), ts)
      case e: TermTree.RecursiveAbstraction =>
        val (b, ts) = rewrite(e.definition, types)
        (Syntax(TermTree.RecursiveAbstraction(e.name, e.ascription, b), tree.span), ts)
      case _ =>
        (tree, types)

    val tsWithChild = if optimizedChild != tree then
        updatedTypes.updated(optimizedChild, types(tree))
      else 
        updatedTypes
    
    normalize(optimizedChild, tsWithChild)  match
      case Some((normalized, tsWithNorm)) =>
        return rewrite(normalized, tsWithNorm)
      case None => 
    

    constantFold(optimizedChild, tsWithChild) match
      case Some((folded, tsWithFold)) =>
        return rewrite(folded, tsWithFold)
      case None =>
        (optimizedChild, updatedTypes)

    propagateConstants(optimizedChild, tsWithChild) match
      case Some((propagated, tsWithProp)) =>
        return rewrite(propagated, tsWithProp)
      case None =>

    inlineFunctions(optimizedChild, tsWithChild) match
      case Some((inlined, tsWithInline)) =>
        return rewrite(inlined, tsWithInline)
      case None =>
        (optimizedChild, tsWithChild)
    

    eliminateDeadCode(optimizedChild, tsWithChild) match
      case Some((eliminated, tsWithElimination)) => 
        return rewrite(eliminated, tsWithElimination)
      case None => 
        (optimizedChild, tsWithChild)
    
  }

  /** Normalize the syntax tree by floating bindings and shifting constants left */
  private def normalize(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =
    import TermTree.TermApplication as F
    import TermTree.Binding as B
    val originalType = types(tree)

    tree.value match
      // Binding is in the RHS of TermApplication: f (let x = 1; b) => let x = 1; f b
      case F(f, Syntax(B(name, initializer, body), _)) =>
        val newApp = Syntax(F(f, body), tree.span)
        val newTree = Syntax(B(name, initializer, newApp), tree.span)
        val ts = types.updated(newApp, originalType).updated(newTree, originalType)
        Some((newTree, ts))
      // Binding is in the LHS of TermApplication: (let x = 1; b) a => let x = 1; b a
      case F(Syntax(B(name, initializer, body), _), a) =>
        val newApp = Syntax(F(body, a), tree.span)
        val newTree = Syntax(B(name, initializer, newApp), tree.span)
        val ts = types.updated(newApp, originalType).updated(newTree, originalType)
        Some((newTree, ts))
      // Commutativity for Add: x + c => c + x (c is a constant, x is not)
      case F(app @ Syntax(F(op @ InfixOperator(f), x), _), IntegerConstant(c)) 
          if f == InfixOperator.Add && !x.value.isInstanceOf[TermTree.IntegerLiteral] =>
        val constant = Syntax(TermTree.IntegerLiteral(c), tree.span)
        val newInner = Syntax(F(op, constant), app.span)
        val newTree = Syntax(F(newInner, x), tree.span)
        val ts = types.updated(constant, Type.Ground.Int)
                      .updated(newInner, types(app))
                      .updated(newTree, originalType)
        Some((newTree, ts))
      // Assossiacivity for Add: (c1 + x) + c2 => (c1 + c2) + x (cX are constants, x is not)
      case F(appOut @ Syntax(F(op1 @ InfixOperator(f1), Syntax(F(appInner @ Syntax(F(op2 @ InfixOperator(f2), IntegerConstant(c1)), _), x), _)), _), IntegerConstant(c2))
          if f1 == InfixOperator.Add && f2 == InfixOperator.Add =>
        val constant = Syntax(TermTree.IntegerLiteral(c1 + c2), tree.span)
        val newInner = Syntax(F(op1, constant), appInner.span)
        val newTree = Syntax(F(newInner, x), tree.span)
        val ts = types.updated(constant, Type.Ground.Int)
                      .updated(newInner, types(appOut)) 
                      .updated(newTree, originalType)
        Some((newTree, ts))
      case _ => None

  /** Returns a literal denoting the result of `tree` iff it represents a constant expression. */
  private def constantFold(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =
    import TermTree.TermApplication as F
    tree.value match
      case F(Syntax(F(InfixOperator(f), IntegerConstant(lhs)), _), IntegerConstant(rhs)) =>
        f match
          case InfixOperator.Add | InfixOperator.Sub | InfixOperator.Mul | InfixOperator.Div =>
            val n = f match
              case InfixOperator.Add => lhs + rhs
              case InfixOperator.Sub => lhs - rhs
              case InfixOperator.Mul => lhs * rhs
              case InfixOperator.Div => lhs / rhs
              case _ => 0

            val newTree = Syntax(TermTree.IntegerLiteral(n), tree.span)
            val ts = types.updated(newTree, Type.Ground.Int)
            Some((newTree, ts))
            
          case _ =>
            val b = f match 
              case InfixOperator.Eq  => lhs == rhs
              case InfixOperator.Neq => lhs != rhs
              case InfixOperator.Lt  => lhs < rhs
              case InfixOperator.Lte => lhs <= rhs
              case InfixOperator.Gt  => lhs > rhs
              case InfixOperator.Gte => lhs >= rhs
              case _ => false

            val newTree = Syntax(TermTree.BooleanLiteral(b), tree.span)
            val ts = types.updated(newTree, Type.Ground.Bool)
            Some((newTree, ts))
      case F(Syntax(F(InfixOperator(f), Syntax(BooleanLiteral(lhs), _)), _), Syntax(BooleanLiteral(rhs), _)) =>
        f match
          case InfixOperator.And | InfixOperator.Or =>
            val n = f match
              case InfixOperator.Eq => lhs && rhs
              case InfixOperator.Neq => lhs || rhs
              case _ => false

            val newTree = Syntax(TermTree.BooleanLiteral(n), tree.span)
            val ts = types.updated(newTree, Type.Ground.Int)
            Some((newTree, ts))
          case _ => None
      case _ => None

  /** Eliminates dead code (useless bindings and hardcoded conditionals) */
  private def eliminateDeadCode(
    tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =
    val originalType = types(tree)

    tree.value match
      case TermTree.Conditional(Syntax(TermTree.BooleanLiteral(true), _), success, _) =>
        Some((success, types.updated(success, originalType)))
      case TermTree.Conditional(Syntax(TermTree.BooleanLiteral(false), _), _, failure) =>
        Some((failure, types.updated(failure, originalType)))
      case TermTree.Binding(name, _, body) if !isFreeIn(name.value.name, body) =>
        Some((body, types.updated(body, originalType)))
      case _ => None
    
  /** Checks if a variable name occurs free (unshadowed) inside a term tree */
  private def isFreeIn(variableName: String, tree: Syntax[TermTree]): Boolean =
    tree.value match
      case TermTree.Variable(name) => name == variableName
      case TermTree.TermAbstraction(parameter, _, body) =>
        if parameter.value.name == variableName then false else isFreeIn(variableName, body)
      case TermTree.Binding(name, initializer, body) =>
        isFreeIn(variableName, initializer) || (if name.value.name == variableName then false else isFreeIn(variableName, body))
      case TermTree.RecursiveAbstraction(name, _, body) =>
        if name.value.name == variableName then false else isFreeIn(variableName, body)
      case TermTree.TermApplication(abstraction, argument) =>
        isFreeIn(variableName, abstraction) || isFreeIn(variableName, argument)
      case TermTree.TypeAbstraction(_, body) =>
        isFreeIn(variableName, body)
      case TermTree.TypeApplication(abstraction, _) =>
        isFreeIn(variableName, abstraction)
      case TermTree.Conditional(condition, success, failure) =>
        isFreeIn(variableName, condition) || isFreeIn(variableName, success) || isFreeIn(variableName, failure)
      case _ => false

  /** Recursively substitutes free occurrences of `target` with `replacement` in `tree` */
  private def substitute(
      target: String, replacement: Syntax[TermTree], tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): (Syntax[TermTree], TypedProgram.TypeAssignments) = {
    if !isFreeIn(target, tree) then return (tree, types)

    val originalType = types(tree)

    val (newTree, newTypes) = tree.value match
      case TermTree.Variable(n) if n == target =>
        val newConst = Syntax(replacement.value, tree.span)
        (newConst, types.updated(newConst, originalType))
      case TermTree.TermAbstraction(p, a, b) =>
        val (newB, ts) = if p.value.name == target then (b, types) else substitute(target, replacement, b, types)
        (Syntax(TermTree.TermAbstraction(p, a, newB), tree.span), ts)
      case TermTree.Binding(n, i, b) =>
        val (newI, ts1) = substitute(target, replacement, i, types)
        val (newB, ts2) = if n.value.name == target then (b, ts1) else substitute(target, replacement, b, ts1)
        (Syntax(TermTree.Binding(n, newI, newB), tree.span), ts2)
      case TermTree.TermApplication(f, a) =>
        val (newF, ts1) = substitute(target, replacement, f, types)
        val (newA, ts2) = substitute(target, replacement, a, ts1)
        (Syntax(TermTree.TermApplication(newF, newA), tree.span), ts2)
      case TermTree.Conditional(c, s, f) =>
        val (newC, ts1) = substitute(target, replacement, c, types)
        val (newS, ts2) = substitute(target, replacement, s, ts1)
        val (newF, ts3) = substitute(target, replacement, f, ts2)
        (Syntax(TermTree.Conditional(newC, newS, newF), tree.span), ts3)
      case TermTree.TypeApplication(a, t) =>
        val (newA, ts1) = substitute(target, replacement, a, types)
        (Syntax(TermTree.TypeApplication(newA, t), tree.span), ts1)
      case TermTree.TypeAbstraction(p, b) =>
        val (newB, ts1) = substitute(target, replacement, b, types)
        (Syntax(TermTree.TypeAbstraction(p, newB), tree.span), ts1)
      case TermTree.RecursiveAbstraction(n, a, d) =>
        val (newD, ts1) = if n.value.name == target then (d, types) else substitute(target, replacement, d, types)
        (Syntax(TermTree.RecursiveAbstraction(n, a, newD), tree.span), ts1)
      case _ => (tree, types)

    if newTree != tree then
      (newTree, newTypes.updated(newTree, originalType))
    else
      (tree, types)
  }

  /** Propagates constants by substituting them into the bodies of their bindings */
  private def propagateConstants(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =
    val originalType = types(tree)

    tree.value match
      case TermTree.Binding(name, init, body) if isConstant(init) && isFreeIn(name.value.name, body) =>
        val (newBody, ts) = substitute(name.value.name, init, body, types)
        val newTree = Syntax(TermTree.Binding(name, init, newBody), tree.span)
        Some((newTree, ts.updated(newTree, originalType)))
      case _ => None

  /** Inline function application */
  private def inlineFunctions(
    tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =
    val originalType = types(tree)

    tree.value match
      case TermTree.TermApplication(
        Syntax(TermTree.TermAbstraction(parameter, _, body), _),
        argument
      ) => 
        val (newBody, ts) = substitute(parameter.value.name, argument, body, types)

        Some((newBody, ts.updated(newBody, originalType)))
      case _ => None    

end Optimizer

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant

private def isConstant(tree: Syntax[TermTree]): Boolean =
  tree.value match
    case _: TermTree.IntegerLiteral => true
    case _: TermTree.BooleanLiteral => true
    case TermTree.UnitLiteral => true
    case _ => false  