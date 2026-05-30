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
    
    eliminateCommonSubexpression(optimizedChild, tsWithChild) match
      case Some((cseTree, tsWithCse)) =>
        return rewrite(cseTree, tsWithCse)
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
              case InfixOperator.And => lhs && rhs
              case InfixOperator.Or => lhs || rhs
              case _ => false

            val newTree = Syntax(TermTree.BooleanLiteral(n), tree.span)
            val ts = types.updated(newTree, Type.Ground.Bool)
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

  /** Eliminate common subexpressions by hoisting them into a local binding */
  private def eliminateCommonSubexpression(
    tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =
    val candidates = collectComputeSubtrees(tree, types)
    val duplicates = findDuplicates(candidates)

    val bVars = boundVars(tree)

    val safeDuplicate = duplicates.find(d => freeVars(d).intersect(bVars).isEmpty)

    safeDuplicate match
      case Some(dup) =>
        val cseName = nextCseName(tree)
        val nameSyntax = Syntax(TermTree.Variable(cseName), dup.span)

        val (newBody, ts1) = replaceStructurally(tree, dup, nameSyntax, types)

        val bindingSyntax = Syntax(
          TermTree.Binding(
            Syntax(TermTree.Variable(cseName), dup.span),
            dup,
            newBody
          ),
          tree.span
        )

        val dupType = types(dup)
        val ts2 = ts1
          .updated(nameSyntax, dupType)
          .updated(bindingSyntax, types(tree))

        Some((bindingSyntax, ts2))

      case None => None    

  /** Duplicate findiing with structural hash grouping */
  private def findDuplicates(trees: List[Syntax[TermTree]]): List[Syntax[TermTree]] =
    val buckets = trees.groupBy(structuralHash)

    val potentialDuplicates = buckets.values.filter(_.size > 1)

    potentialDuplicates.flatMap { bucket =>
      bucket.foldLeft(List.empty[Syntax[TermTree]]) { (acc, tree) =>
        if (acc.exists(t => isStructurallyEqual(t, tree))) acc
        else tree :: acc
      }.filter { uniqueTree =>
        bucket.count(t => isStructurallyEqual(t, uniqueTree)) > 1
      }
    }.toList

  /** Computes a structural hash */
  private def structuralHash(tree: Syntax[TermTree]): Int = tree.value match
    case TermTree.Variable(n) => n.hashCode()
    case TermTree.IntegerLiteral(v) => v.hashCode()
    case TermTree.BooleanLiteral(v) => v.hashCode
    case TermTree.UnitLiteral => 17
    case TermTree.TermApplication(f, a) =>
      (structuralHash(f) * 31) ^ structuralHash(a)
    case _ => 0

  /** Structural equality matching the exact same shape */
  private def isStructurallyEqual(a: Syntax[TermTree], b: Syntax[TermTree]): Boolean = (a.value, b.value) match
    case (TermTree.Variable(n1), TermTree.Variable(n2)) => n1 == n2
    case (TermTree.IntegerLiteral(v1), TermTree.IntegerLiteral(v2)) => v1 == v2
    case (TermTree.BooleanLiteral(v1), TermTree.BooleanLiteral(v2)) => v1 == v2
    case (TermTree.UnitLiteral, TermTree.UnitLiteral) => true
    case (TermTree.TermApplication(f1, a1), TermTree.TermApplication(f2, a2)) =>
      isStructurallyEqual(f1, f2) && isStructurallyEqual(a1, a2)
    case _ => false
    
  /** Recursively replaces identical subtrees structurally */
  private def replaceStructurally(
    tree: Syntax[TermTree], target: Syntax[TermTree], replacement: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): (Syntax[TermTree], TypedProgram.TypeAssignments) =
    if (isStructurallyEqual(tree, target)) {
      val newTree = Syntax(replacement.value, tree.span)
      return (newTree, types.updated(newTree, types(tree)))
    }

    val originalType = types(tree)

    val (newTree, newTypes) = tree.value match
      case TermTree.TermApplication(f, a) =>
        val (newF, ts1) = replaceStructurally(f, target, replacement, types)
        val (newA, ts2) = replaceStructurally(a, target, replacement, ts1)
        (Syntax(TermTree.TermApplication(newF, newA), tree.span), ts2)
      case TermTree.Conditional(c, s, f) =>
        val (newC, ts1) = replaceStructurally(c, target, replacement, types)
        val (newS, ts2) = replaceStructurally(s, target, replacement, ts1)
        val (newF, ts3) = replaceStructurally(f, target, replacement, ts2)
        (Syntax(TermTree.Conditional(newC, newS, newF), tree.span), ts3)
      case TermTree.Binding(n, i, b) =>
        val (newI, ts1) = replaceStructurally(i, target, replacement, types)
        val (newB, ts2) = replaceStructurally(b, target, replacement, ts1)
        (Syntax(TermTree.Binding(n, newI, newB), tree.span), ts2)
      case TermTree.TermAbstraction(p, a, b) =>
        val (newB, ts1) = replaceStructurally(b, target, replacement, types)
        (Syntax(TermTree.TermAbstraction(p, a, newB), tree.span), ts1)
      case TermTree.TypeAbstraction(p, b) =>
        val (newB, ts1) = replaceStructurally(b, target, replacement, types)
        (Syntax(TermTree.TypeAbstraction(p, newB), tree.span), ts1)
      case TermTree.TypeApplication(a, t) =>
        val (newA, ts1) = replaceStructurally(a, target, replacement, types)
        (Syntax(TermTree.TypeApplication(newA, t), tree.span), ts1)
      case TermTree.RecursiveAbstraction(n, a, d) =>
        val (newD, ts1) = replaceStructurally(d, target, replacement, types)
        (Syntax(TermTree.RecursiveAbstraction(n, a, newD), tree.span), ts1)
      case _ => (tree, types)

    if (newTree != tree) (newTree, newTypes.updated(newTree, originalType))
    else (tree, types)
    

  /** Collects applications representing pure mathematical / functions computations */
  private def collectComputeSubtrees(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments): List[Syntax[TermTree]] =
    val child = tree.value match
      case TermTree.TermApplication(f, a) => collectComputeSubtrees(f, types) ::: collectComputeSubtrees(a, types)
      case TermTree.Conditional(c, s, f) => collectComputeSubtrees(c, types) ::: collectComputeSubtrees(s, types) ::: collectComputeSubtrees(f, types)
      case TermTree.Binding(_, i, b) => collectComputeSubtrees(i, types) ::: collectComputeSubtrees(b, types)
      case TermTree.TermAbstraction(_, _, b) => collectComputeSubtrees(b, types)
      case TermTree.TypeAbstraction(_, b) => collectComputeSubtrees(b, types)
      case TermTree.TypeApplication(f, _) => collectComputeSubtrees(f, types)
      case TermTree.RecursiveAbstraction(_, _, d) => collectComputeSubtrees(d, types)
      case _ => Nil

    val isGround = types.get(tree).exists(_.isInstanceOf[Type.Ground])

    if (isPureCompute(tree) && tree.value.isInstanceOf[TermTree.TermApplication] && isGround) then
      tree :: child
    else 
      child

  private def isPureCompute(tree: Syntax[TermTree]): Boolean = tree.value match
    case _: TermTree.Variable | _: TermTree.IntegerLiteral | _: TermTree.BooleanLiteral | TermTree.UnitLiteral => true
    case TermTree.TermApplication(f, a) => isPureCompute(f) && isPureCompute(a)
    case _ => false 
  
  private def boundVars(tree: Syntax[TermTree]): Set[String] = tree.value match
    case TermTree.TermAbstraction(p, _, b) => boundVars(b) + p.value.name
    case TermTree.Binding(n, i, b) => boundVars(i) ++ boundVars(b) + n.value.name
    case TermTree.RecursiveAbstraction(n, _, d) => boundVars(d) + n.value.name
    case TermTree.TermApplication(f, a) => boundVars(f) ++ boundVars(a)
    case TermTree.Conditional(c, s, f) => boundVars(c) ++ boundVars(s) ++ boundVars(f)
    case TermTree.TypeAbstraction(_, b) => boundVars(b)
    case TermTree.TypeApplication(f, _) => boundVars(f)
    case _ => Set.empty
  
  private def freeVars(tree: Syntax[TermTree]): Set[String] = tree.value match
    case TermTree.Variable(name) => Set(name)
    case TermTree.TermAbstraction(p, _, b) => freeVars(b) - p.value.name
    case TermTree.Binding(n, i, b) => freeVars(i) ++ (freeVars(b) - n.value.name)
    case TermTree.RecursiveAbstraction(n, _, d) => freeVars(d) - n.value.name
    case TermTree.TermApplication(f, a) => freeVars(f) ++ freeVars(a)
    case TermTree.Conditional(c, s, f) => freeVars(c) ++ freeVars(s) ++ freeVars(f)
    case TermTree.TypeAbstraction(_, b) => freeVars(b)
    case TermTree.TypeApplication(f, _) => freeVars(f)
    case _ => Set.empty

  private def nextCseName(tree: Syntax[TermTree]): String =
    val vars = allVars(tree)
    def loop(i: Int): String =
      val name = s"cse_$i"
      if (vars.contains(name)) loop(i + 1) else name 
    
    loop(0)

  private def allVars(tree: Syntax[TermTree]): Set[String] = tree.value match
    case TermTree.Variable(n) => Set(n)
    case TermTree.TermAbstraction(p, _, b) => allVars(b) + p.value.name
    case TermTree.Binding(n, i, b) => allVars(i) ++ allVars(b) + n.value.name
    case TermTree.RecursiveAbstraction(n, _, d) => allVars(d) + n.value.name
    case TermTree.TermApplication(f, a) => allVars(f) ++ allVars(a)
    case TermTree.Conditional(c, s, f) => allVars(c) ++ allVars(s) ++ allVars(f)
    case TermTree.TypeAbstraction(_, b) => allVars(b)
    case TermTree.TypeApplication(f, _) => allVars(f)
    case _ => Set.empty

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