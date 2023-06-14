package playground.data.link

import scala.util.control.TailCalls.{TailRec, done, tailcall}
import scala.collection.View.Empty

package tuples:
    private def believeMe[A, B]: A =:= B = <:<.refl.asInstanceOf[A =:= B]

    def concatMapCommutes[T1 <: Tuple, T2 <: Tuple, X[_]]
        : Tuple.Concat[Tuple.Map[T1, X], Tuple.Map[T2, X]] =:= Tuple.Map[Tuple.Concat[T1, T2], X] =
        believeMe

    def mapIdent[T <: Tuple]: Tuple.Map[T, [x] =>> x] =:= T = believeMe

end tuples

abstract class Void[I, O]:
    def absurd[X]: X

enum Link[+P[_, _], O <: Tuple]:
    case Rock                                                                  extends Link[Nothing, EmptyTuple]
    case Glue[+P[_, _], T1 <: Tuple, T2 <: Tuple](init: Link[P, T1], tail: Link[P, T2])
        extends Link[P, Tuple.Concat[T1, T2]]
    case Pass[+P[_, _], I <: Tuple, O <: Tuple](op: P[I, O], args: Link[P, I]) extends Link[P, O]

    def cata[X[_]](f: Link.Cata[P, X]): Tuple.Map[O, X]              = catatr(f.kmap([A] => (x: A) => done(x))).result
    def catatr[X[_]](f: Link.CataTR[P, X]): TailRec[Tuple.Map[O, X]] =
        this match
            case Rock             => done(EmptyTuple)
            case Glue(init, tail) =>
                for
                    ir <- init.catatr(f)
                    tr <- tail.catatr(f)
                yield tuples.concatMapCommutes.apply(ir ++ tr)

            case Pass(op, args) =>
                for
                    argr <- args.catatr(f)
                    or   <- f.exec(op, argr)
                yield or
end Link

object Link:

    class OutApp[O]:
        inline def apply[R](x: R)(using Tuple1[R] <:< O): O = Tuple1(x)

    trait CataGen[-P[_, _], X[_], F[_]]:
        self =>
        def exec[I <: Tuple, O <: Tuple](p: P[I, O], x: Tuple.Map[I, X]): F[Tuple.Map[O, X]]

        def kmap[G[_]](f: [A] => F[A] => G[A]): CataGen[P, X, G] = new:
            def exec[I <: Tuple, O <: Tuple](p: P[I, O], x: Tuple.Map[I, X]): G[Tuple.Map[O, X]] =
                f(self.exec(p, x))

        inline def or[Q[_, _]](using that: CataGen[Q, X, F]): CataGen[[x, y] =>> P[x, y] | Q[x, y], X, F] = new:
            override def exec[I <: Tuple, O <: Tuple](pq: P[I, O] | Q[I, O], x: Tuple.Map[I, X]): F[Tuple.Map[O, X]] =
                pq match
                    case p: P[I, O] => self.exec(p, x)
                    case q: Q[I, O] => that.exec(q, x)

        def Out[O <: Tuple] = OutApp[Tuple.Map[O, X]]
    end CataGen

    type CataTR[-P[_, _], X[_]] = CataGen[P, X, TailRec]
    type Cata[-P[_, _], X[_]]   = CataGen[P, X, [x] =>> x]

    type Eval[-P[_, _]] = Cata[P, [x] =>> x]

    class NoCata[X[_], F[_]] extends CataGen[Void, X, F]:
        def exec[I <: Tuple, O <: Tuple](void: Void[I, O], x: Tuple.Map[I, X]) = void.absurd

    val noEval: NoCata[[x] =>> x, [x] =>> x] = NoCata()
end Link

type |/|[P1[_, _], P2[_, _]] = [A, B] =>> P1[A, B] | P2[A, B]

//mixin for language companions
trait Linking[P[_, _]]:
    type Op0[A]            = P[EmptyTuple, Tuple1[A]]
    type Op1[A, B]         = P[Tuple1[A], Tuple1[B]]
    type Op[T <: Tuple, A] = P[T, Tuple1[A]]

    // DSL for function-like composing AST
    extension [B <: Tuple](p: P[EmptyTuple, B]) def apply(): Link[P, B] = Link.Pass(p, Link.Rock)
    extension [A <: Tuple, B <: Tuple](p: P[A, B])
        def apply[P1[_, _]](args: Link[P1, A]): Link[P |/| P1, B] = Link.Pass(p, args)
        def apply[P1[_, _], P2[_, _], A1 <: Tuple, A2 <: Tuple](arg1: Link[P1, A1], arg2: Link[P2, A2])(using
            ev: Tuple.Concat[A1, A2] =:= A
        ): Link[P |/| P1 |/| P2, B] =
            Link.Pass(p, ev.liftCo[[x] =>> Link[P1 |/| P2, x & Tuple]](Link.Glue(arg1, arg2)))

        def apply[P1[_, _], P2[_, _], P3[_, _], A1 <: Tuple, A2 <: Tuple, A3 <: Tuple](
            arg1: Link[P1, A1],
            arg2: Link[P2, A2],
            arg3: Link[P3, A3]
        )(using
            ev: Tuple.Concat[Tuple.Concat[A1, A2], A3] =:= A
        ): Link[P |/| P1 |/| P2 |/| P3, B] =
            Link.Pass(p, ev.liftCo[[x] =>> Link[P1 |/| P2 |/| P3, x & Tuple]](Link.Glue(Link.Glue(arg1, arg2), arg3)))
    end extension

    trait Evaluator extends Link.Cata[P, [x] =>> x]:
        def run[I, O](p: P[I, O], in: I): O
        override def exec[I <: Tuple, O <: Tuple](p: P[I, O], x: Tuple.Map[I, [x] =>> x]): Tuple.Map[O, [x] =>> x] =
            tuples.mapIdent[O].flip(run[I, O](p, tuples.mapIdent[I](x)))

    type Shower = Link.Cata[P, [x] =>> String]
end Linking
