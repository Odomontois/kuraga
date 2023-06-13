package playground.data.link

import scala.util.control.TailCalls.{TailRec, done, tailcall}
import scala.collection.View.Empty

package tuples:
    private def believeMe[A, B]: A =:= B = <:<.refl.asInstanceOf[A =:= B]

    def concatMapCommutes[T1 <: Tuple, T2 <: Tuple, X[_]]
        : Tuple.Concat[Tuple.Map[T1, X], Tuple.Map[T2, X]] =:= Tuple.Map[Tuple.Concat[T1, T2], X] =
        believeMe

end tuples

enum Link[+P[_ <: Tuple, _], O <: Tuple]:
    case Rock                                                                           extends Link[Nothing, EmptyTuple]
    case Glue[+P[_ <: Tuple, _], T1 <: Tuple, T2 <: Tuple](init: Link[P, T1], tail: Link[P, T2])
        extends Link[P, Tuple.Concat[T1, T2]]
    case Pass[+P[_ <: Tuple, _], I <: Tuple, O <: Tuple](op: P[I, O], args: Link[P, I]) extends Link[P, O]

    def catatr[X[_]](
        f: [x <: Tuple, y <: Tuple] => P[x, y] => (Tuple.Map[x, X] => TailRec[Tuple.Map[y, X]])
    ): TailRec[Tuple.Map[O, X]] =
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
                    or   <- f(op)(argr)
                yield or
end Link
