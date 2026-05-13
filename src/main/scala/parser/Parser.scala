package yafl.parser

import yafl.{Diagnostic, SourceFile, SourceSpan}
import yafl.syntax.{Syntax, TermTree, TypeTree}

object Parser:

  /** The context in which parsing is taking place.
    *
    * @param source The source file being parsed.
    * @param position The position of the parser in the source file.
    */
  case class Context(source: SourceFile, position: SourceFile.Index):

    /** Returns a copy of `this` advanced to the position immediately after `t`. */
    def after(t: Token): Context =
      this.copy(position = t.span.end)

  end Context

  /** The result of a parsing method.
    *
    * A result is essentially a pair composed of a value, typically parsed from the input source,
    * and a context denoting the state of the parser. The latter can be understood as the "state"
    * of the parser, which is meant to flow into parsing methods.
    */
  type Result[+T] = yafl.Result[T, Context]

  /** Parses the program written in `source`. */
  def parse(source: SourceFile): Syntax[TermTree] =
    val parsed = term(using Context(source, source.start))
    if peek(using parsed.state).isDefined then
      throw expected("end of input")(using parsed.state)
    parsed.value

  /** Parses a term. */
  private def term(using Context): Result[Syntax[TermTree]] =
    termApplication(0)

  /** Parses a simple term or an application.
    *
    * @param precedence The minimum precedence level of the operators way may considerate.
    */
  private def termApplication(precedence: Int)(using Context): Result[Syntax[TermTree]] = {
    // The following loop implements precedence climbing. At each iteration, we look for an infix
    // operator `f` after `lhs` whose precedence is not stronger than the current precedence level.
    // If there's one, we parse it followed by a right hand side, which is either a simple term or
    // the result of applying an infix operator with a stronger precedence. If there isn't one but
    // there are still tokens to parse, we parse a term application. Otherwise we return the term
    // we have computed so far.
    def loop(lhs: Syntax[TermTree])(using Context): Result[Syntax[TermTree]] = peek match
      case Some(t) if (t.tag == Token.operator) =>
        if (precedence <= t.precedence) then
          // We found an operator with enough precedence. We'll take it along with a right operand
          // and keep parsing for the rest of the expression.
          infixOperator
            .and { (f) =>
              termApplication(t.precedence + 1).map { (rhs) =>
                val s = lhs.span.extendedToCover(f.span)
                val a = Syntax(TermTree.TermApplication(f, lhs), s)
                Syntax(TermTree.TermApplication(a, rhs), s.extendedToCover(rhs.span))
              }
            }
            .and(loop)
        else
          // We found an operator but it doesn't have enough precedence. We're done.
          result(lhs)

      case Some(t) if !t.isDelimiter =>
        // The next token isn't an operator so we'll treat it as the start of a term occurring as
        // the argument of some application.
        typeApplication.map { (rhs) =>
          Syntax(TermTree.TermApplication(lhs, rhs), lhs.span.extendedToCover(rhs.span))
        }

      case _ =>
        // We reached a delimiter or the end of the steam. We're done.
        result(lhs)

    prefixTerm.and(loop)
  }

  /** Parses a simple term of the application of a prefix operator. */
  private def prefixTerm(using Context): Result[Syntax[TermTree]] =
    peek.map((t) => t.tag) match
      case Some(Token.operator) =>
        prefixOperator.and { op =>
          prefixTerm.map { rhs =>
            Syntax(
              TermTree.TermApplication(op, rhs),
              op.span.extendedToCover(rhs.span)
            )
          }
        }
      case _ => typeApplication

  /** Parses a simple term or a type application. */
  private def typeApplication(using Context): Result[Syntax[TermTree]] =
    simpleTerm

  /** Parses a simple term. */
  private def simpleTerm(using Context): Result[Syntax[TermTree]] =
    peek.map((t) => t.tag) match
      case Some(Token.boolean) => booleanLiteral
      case Some(Token.integer) => integerLiteral
      case Some(Token.identifier) => termIdentifier
      case Some(Token.leftParenthesis) => lambdaOrParenthesized
      case Some(Token.`if`) => conditional
      case Some(Token.let) => binding
      case Some(Token.leftBracket) => typeAbstraction
      case _ => throw expected("term")

  /** Parses a Boolean literal. */
  private def booleanLiteral(using Context): Result[Syntax[TermTree.BooleanLiteral]] =
    take(Token.boolean, "Boolean literal")
      .map((n) => Syntax(TermTree.BooleanLiteral(n.text == "true"), n.span))

  /** Parses an integer literal. */
  private def integerLiteral(using Context): Result[Syntax[TermTree.IntegerLiteral]] =
    take(Token.integer, "integer literal")
      .map((n) => Syntax(TermTree.IntegerLiteral(n.text.toString.toInt), n.span))

  /** Parses a term identifier. */
  private def termIdentifier(using Context): Result[Syntax[TermTree.Variable]] =
    take(Token.identifier, "identifier")
      .map((n) => Syntax(TermTree.Variable(n.text.toString), n.span))

  /** Parses an infix operator. */
  private def infixOperator(using Context): Result[Syntax[TermTree.Variable]] =
    take(Token.operator, "operator")
      .map((n) => Syntax(TermTree.Variable(s"infix${n.text}"), n.span))

  /** Parses a prefix operator */
  private def prefixOperator(using Context): Result[Syntax[TermTree.Variable]] =
    take(Token.operator, "operator")
      .map((n) => Syntax(TermTree.Variable(s"prefix${n.text}"), n.span))

  /** Parses a lambda or a parenthesized term. */
  private def lambdaOrParenthesized(using Context): Result[Syntax[TermTree]] =
    take(Token.leftParenthesis, "'('").and { (opener) =>
      // If the next token is a closing parenthesis, we parse a unit literal. Otherwise, we may be
      // parsing either a lambda or simply a parenthesized term, depending on the presence of a
      // thick arrow after the closing parenthesis.
      takeIf(Token.hasTag(Token.rightParenthesis)) match
        case Some(s) =>
          // We've pased a closing parenthesis right after the opening one.
          s.map((end) => Syntax(TermTree.UnitLiteral, opener.span.extendedToCover(end.span)))

        case _ =>
          // If the next token is an identifier followed by a colon, we'll parse it as a parameter
          // and parse the rest of a lambda. Otherwise we'll expect to parse an arbitrary term
          // followed by a closing parenthesis.
          term.and { (parameterOrTerm) =>
            (parameterOrTerm, takeIf(Token.hasTag(Token.colon))) match
              case (Syntax(n: TermTree.Variable, s), Some(c)) =>
                // `parameterOrTerm` is actually a parameter declaration. We have to parse its
                // ascription, which may be followed by multiple parameters.
                typ3(using c.state)
                  .and((t) => trailingTermParameters(List((Syntax(n, s), t))))
                  .andDiscard(take(Token.rightParenthesis, "')'"))
                  .andDiscard(take(Token.thickArrow, "'=>'"))
                  .and((ps) => term.map { (body) =>
                    ps.foldLeft(body) { (b, p) =>
                      val (x, t) = p
                      Syntax(TermTree.TermAbstraction(x, t, b), x.span.extendedToCover(b.span))
                    }
                  })

              case _ =>
                // `parameterOrTerm` is just a term; parse the closing parenthesis.
                take(Token.rightParenthesis, "')'").map((_) => parameterOrTerm)
          }
    }

  /** Parses a conditional expression ('if' term 'then' term 'else' term) */
  private def conditional(using Context): Result[Syntax[TermTree.Conditional]] =
    take(Token.`if`, "'if'").and { start => 
      term.andDiscard(take(Token.`then`, "'then'")).and { condition => 
        term.andDiscard(take(Token.`else`, "'else'")).and { success =>
          term.map { failure => 
            Syntax(
              TermTree.Conditional(condition, success, failure),
              start.span.extendedToCover(failure.span)
            )
          }
        }
      }
    }

  /** Parses a binding (let identifier = term; term) */
  private def binding(using Context): Result[Syntax[TermTree.Binding]] =
    take(Token.let, "'let'").and { start =>
      termIdentifier.andDiscard(take(Token.equal, "'='")).and { name => 
        term.andDiscard(take(Token.semicolon, "';'")).and { initializer => 
          term.map { body =>
            Syntax(
              TermTree.Binding(name, initializer, body),
              start.span.extendedToCover(body.span)
            )
          }
        }
      }
    }

  /** Parses a type abstraction ([T] => term) */
  private def typeAbstraction(using Context): Result[Syntax[TermTree.TypeAbstraction]] =
    take(Token.leftBracket, "'['").and { start => 
      typeIdentifier
        .andDiscard(take(Token.rightBracket, "']'"))
        .andDiscard(take(Token.thickArrow, "'=>'"))
        .and { parameter =>
          term.map {body => 
            Syntax(
              TermTree.TypeAbstraction(parameter, body),
              start.span.extendedToCover(body.span)
            )
          }
        }
    }

  /** The name of a parameter and its ascription. */
  private type Parameter = (Syntax[TermTree.Variable], Syntax[TypeTree])

  /** Parses a (possibly empty) list of parameters, each prefixed by a leading comma. */
  private def trailingTermParameters(
      ps: List[Parameter]
  )(using Context): Result[List[Parameter]] =
    takeIf(Token.hasTag(Token.comma)) match
      case Some(separator) =>
        termIdentifier(using separator.state)
          .andDiscard(take(Token.colon, "':'"))
          .andCombine(typ3)
          .and(p => trailingTermParameters(p :: ps))
      case _ => result(ps)

  /** Parses a type. */
  private def typ3(using Context): Result[Syntax[TypeTree]] =
    simpleType.and { domain =>
      peek.map((t) => t.tag) match
        case Some(Token.thinArrow) =>
          take(Token.thinArrow, "'->'").and { arrow =>
            typ3.map { codomain =>
              Syntax(
                TypeTree.Arrow(domain, codomain),
                domain.span.extendedToCover(codomain.span)
              )
            }
          }
        case _ => result(domain)
    }

  /** Parses a simple type. */
  private def simpleType(using Context): Result[Syntax[TypeTree]] =
    peek.map((t) => t.tag) match
      case Some(Token.identifier) => typeIdentifier
      case Some(Token.leftBracket) => universalType
      case Some(Token.leftParenthesis) => parenthesizedType
      case _ => throw expected("type")

  /** Parses a type identifier. */
  private def typeIdentifier(using Context): Result[Syntax[TypeTree.Variable]] =
    take(Token.identifier, "identifier")
      .map((n) => Syntax(TypeTree.Variable(n.text.toString), n.span))

  /** Parses a universal type ([T] => type) */
  private def universalType(using Context): Result[Syntax[TypeTree.ForAll]] =
    take(Token.leftBracket, "'['").and { start =>
      typeIdentifier
        .andDiscard(take(Token.rightBracket, "']'"))
        .andDiscard(take(Token.thickArrow, "']'"))
        .and { parameter =>
          typ3.map { body => 
            Syntax(
              TypeTree.ForAll(parameter, body),
              start.span.extendedToCover(body.span)
            )
          }
        }
    }

  /** Parses a parenthesized type */
  private def parenthesizedType(using Context): Result[Syntax[TypeTree]] =
    take(Token.leftParenthesis, "'('").and { start => 
      typ3.and { body =>
        take(Token.rightParenthesis, "')'").map { end =>
          Syntax(
            body.value,
            start.span.extendedToCover(end.span)
          )
        }
      }
    }

  /** Returns the next token in `source`, if any. */
  private def peek(using Context): Option[Token] =
    context.source.nextToken(context.position)

  /** Returns `true` iff the next token has tag `k`. */
  private def nextIs(k: Token.Tag)(using Context): Boolean =
    peek.map(Token.hasTag(k)).getOrElse(false)

  /** Parses a token. */
  private def take()(using Context): Result[Token] =
    val t = peek.get
    result(t)(using context.after(t))

  /** Parses the next token iff it has tag `k`; otherwise, reports that `s` was expected. */
  private def take(k: Token.Tag, s: String)(using Context): Result[Token] =
    if nextIs(k) then take() else throw expected(s)

  /** Parses the next token iff it satisfies the given predicate; otherwise, returns `None`. */
  private def takeIf(predicate: Token => Boolean)(using Context): Option[Result[Token]] =
    peek match
      case Some(t) if predicate(t) => Some(result(t)(using context.after(t)))
      case _ => None

  /** Returns a parse error reporting that `s` was expected at `site`. */
  private def expected(s: String, site: SourceSpan): Diagnostic =
    Diagnostic(s"expected ${s}", site)

  /** Returns a parse error reporting that `s` was expected at the current position. */
  private def expected(s: String)(using Context): Diagnostic =
    val p = peek.map((t) => t.span.start).getOrElse(context.position)
    expected(s, SourceSpan(p, p, context.source))

  /** Returns the current context. */
  private def context(using ctx: Context): Context =
    ctx

  /** Returns a result wrapping `value` together with the current context. */
  private def result[T](value: T)(using Context): Result[T] =
    yafl.Result(value)

end Parser
