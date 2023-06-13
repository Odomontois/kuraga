package playground.data.link

import cats.kernel.Comparison

class BinOp[A, B]:
    inline def apply[I, C](t: I)(using I <:< (A, B))(inline f: (A, B) => C) =
        val (x, y) = t: (A, B)
        Tuple1(f(x, y))

class UnOp[A]:
    inline def apply[I, B](t: I)(using I <:< Tuple1[A])(inline f: A => B) =
        val Tuple1(x) = t: Tuple1[A]
        Tuple1(f(x))

enum Lits[I, O]:
    case Integ(x: BigInt)  extends Op0[BigInt]
    case Boole(x: Boolean) extends Op0[Boolean]

object Lits extends Linking[Lits]:
    given Evaluator with
        def run[I, O](op: Lits[I, O], i: I): O = op match
            case Integ(x) => Tuple1(x)
            case Boole(x) => Tuple1(x)
end Lits

enum Arith[I, O]:
    case Add extends Op[(BigInt, BigInt), BigInt]
    case Sub extends Op[(BigInt, BigInt), BigInt]
    case Mul extends Op[(BigInt, BigInt), BigInt]

object Arith extends Linking[Arith]:
    given Evaluator with
        def run[I, O](op: Arith[I, O], i: I): O =
            val dop = BinOp[BigInt, BigInt]
            op match
                case Add => dop(i)(_ + _)
                case Sub => dop(i)(_ - _)
                case Mul => dop(i)(_ * _)
end Arith

enum Logi[I, O]:
    case True  extends Op0[Boolean]
    case False extends Op0[Boolean]
    case And   extends Op[(Boolean, Boolean), Boolean]
    case Or    extends Op[(Boolean, Boolean), Boolean]
    case Not   extends Op1[Boolean, Boolean]

object Logi extends Linking[Logi]:
    given Evaluator with
        override def run[I, O](p: Logi[I, O], in: I): O =
            val bop = BinOp[Boolean, Boolean]
            val uop = UnOp[Boolean]
            p match
                case True  => Tuple1(true)
                case False => Tuple1(false)
                case And   => bop(in)(_ && _)
                case Or    => bop(in)(_ || _)
                case Not   => uop(in)(!_)
    end given
end Logi

enum Cond[I, O]:
    case If[A]() extends Op[(Boolean, A, A), A]

object Cond extends Linking[Cond]:
    given Evaluator with
        def run[I, O](op: Cond[I, O], i: I): O = op match
            case _: If[a] =>
                val (b, x, y) = i: (Boolean, a, a)
                Tuple1(if b then x else y)

enum Comparison[I, O]:
    case Lt  extends Op[(BigInt, BigInt), Boolean]
    case Lte extends Op[(BigInt, BigInt), Boolean]
    case Eq  extends Op[(BigInt, BigInt), Boolean]

object Comparison extends Linking[Comparison]:
    given Evaluator with
        def run[I, O](op: Comparison[I, O], i: I): O =
            val dop = BinOp[BigInt, BigInt]
            op match
                case Lt  => dop(i)(_ < _)
                case Lte => dop(i)(_ <= _)
                case Eq  => dop(i)(_ == _)

type ExpQ[I, O] = Lits[I, O] | Arith[I, O] | Logi[I, O] | Cond[I, O] | Comparison[I, O]

type Expr[A] = Link[ExpQ, Tuple1[A]]

val exprCata: Link.Cata[ExpQ, [x] =>> x] = Link.noEval.or[Lits].or[Arith].or[Logi].or[Cond].or[Comparison]

val ltExpr =
    Comparison.Lt(
      Lits.Integ(1)(),
      Lits.Integ(2)()
    )

val sumExpr =
    Arith.Add(
      Lits.Integ(5)(),
      Lits.Integ(6)()
    )

val mulExpr =
    Arith.Mul(
      Lits.Integ(3)(),
      Lits.Integ(4)()
    )

val condExpr = Cond.If()(
  ltExpr,
  sumExpr,
  mulExpr
)

val fullCondExpr = Cond.If()(
  Comparison.Lt(
    Lits.Integ(1)(),
    Lits.Integ(2)()
  ),
  Arith.Add(
    Lits.Integ(5)(),
    Lits.Integ(6)()
  ),
  Arith.Mul(
    Lits.Integ(3)(),
    Lits.Integ(4)()
  )
)


@main def arithRun() = 
    println(fullCondExpr.cata(exprCata)._1)