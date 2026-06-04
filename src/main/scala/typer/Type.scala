package yafl.typer

/** The type of a term.
  *
  * `Type` differs from `TypeTree` in that an instance of the latter is just a syntactic expression
  * denoting an instance of the former. For example, given the input syntax `(x : Int) => x`, the
  * occurrence of `Int` is an instance of `TypeTree.Variable` denoting `Type.Ground.Int`.
  */
sealed trait Type:

  /** Returns `this` recursively transformed by `transform`.
    *
    * The structure of `this` is visited in pre-order. `transform` is applied after all children
    * of a type tree have been visited. As a consequence, `map` is always applied on transformed
    * children. For example, if `this` is an arrow type `Arrow(T, U)`, then `map(this)` evaluates
    * to `transform(Arrow(transform(T), transform(T)))`.
    */
  def map(transform: Type => Type): Type =
    transform(this)

  /** Returns `true` iff `that` occurs in `this`. */
  def contains(that: Type.Variable.Unification): Boolean =
    false

  /** Returns an arrow type from `this` to `that`. */
  def to(that: Type): Type.Arrow =
    Type.Arrow(this, that)

object Type:

  /** The type of an uninterpreted value (e.g., an integer). */
  enum Ground extends Type:

    case Unit, Bool, Int

    override def toString(): String = this match
      case Unit => "Unit"
      case Bool => "Bool"
      case Int => "Int"

  end Ground

  /** A type variable.
    *
    * There are two forms of type variables: bound variables and unification variables. The former
    * denote variables introduced by a binder (e.g., `T` in `[T] => T`) whereas the latter denote
    * free variables, open for unification (e.g., `T` in `() => T`).
    *
    * Bound variables are represented using De Brujin indices and unification variables are
    * identified by a unique number.
    */
  sealed trait Variable extends Type

  object Variable:

    /** A type variable introduced by a bindinder. */
    case class Bound(id: Int) extends Variable:

      override def toString(): String =
        s"α${id}"

    end Bound

    /** A type variable occurring free. */
    case class Unification(id: Int) extends Variable:

      override def contains(that: Type.Variable.Unification): Boolean =
        this == that

      override def toString(): String =
        s"%${id}"

    end Unification

  end Variable

  /** The type of a function. */
  case class Arrow(domain: Type, codomain: Type) extends Type:

    override def map(transform: Type => Type): Type =
      transform(Arrow(domain.map(transform), codomain.map(transform)))

    override def contains(that: Type.Variable.Unification): Boolean =
      domain.contains(that) || domain.contains(that)

    override def toString(): String =
      s"(${domain}) -> ${codomain}"

  end Arrow

  /** The type of a type abstraction. */
  case class ForAll(body: Type) extends Type:

    /** Returns the body of `this` in which occurrences of the type variable introduced by `this`
      * have been replaced by `argument`.
      *
      * For example, if `this` is `Λ.α0 -> U`, then `this(T)` is `T -> U`.
      */
    def apply(argument: Type): Type =
      body.map { (t) => t match
        case Variable.Bound(i) => if i == 0 then argument else Variable.Bound(i - 1)
        case _ => t
      }

    override def map(transform: Type => Type): Type =
      transform(ForAll(body.map(transform)))

    override def contains(that: Type.Variable.Unification): Boolean =
      body.contains(that)

    override def toString(): String =
      def loop(b: Type, s: List[String]): String = b match
        case t: ForAll =>
          loop(t.body, "Λ" :: s)
        case _ =>
          (".${b}" :: s).reverse.mkString
      loop(this, Nil)

  end ForAll

end Type
