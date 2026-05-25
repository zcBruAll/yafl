package yafl.syntax

/** An operator applied with infix notation (e.g., `+` in `1 + 2`). */
enum InfixOperator:

  case Add, Sub, Mul, Div
  case Eq, Neq, Lt, Lte, Gt, Gte

object InfixOperator:

  def unapply(s: Syntax[TermTree]): Option[InfixOperator] =
    s match
      case Syntax(TermTree.Variable(n), _) => n match
        case "infix+" => Some(Add)
        case "infix-" => Some(Sub)
        case "infix*" => Some(Mul)
        case "infix/" => Some(Div)
        case "infix==" => Some(Eq)
        case "infix!=" => Some(Neq)
        case "infix<"  => Some(Lt)
        case "infix<=" => Some(Lte)
        case "infix>"  => Some(Gt)
        case "infix>=" => Some(Gte)
        case _ => None
      case _ => None

end InfixOperator
