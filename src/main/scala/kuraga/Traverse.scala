package kuraga
import Eval.delay

trait Walk[S, T, A, B, -TC[_[_]]]:
    def[F[+_]: TC] (c: S) walkLz(f: A => Eval[F[B]]) : Eval[F[T]]

    def[F[+_]: TC] (c: S) walk(f: A => F[B]) : F[T] = 
       c.walkLz(a => f(a).delay).value



trait Walkable[T[_], A, TC[_[_]]] extends Forall2[[A, B] =>> Walk[T[A], T[B], A, B, TC]]:
    self =>
    def [F[_]: TC, A, B] (fa: T[A]) walkLzA (f: A => Eval[F[B]])  : Eval[F[T[B]]]

    def[F[+_]: TC, A, B] (c: T[A]) walkA(f: A => F[B]) : F[T[B]] = 
        c.walkLzA(a => f(a).delay).value

    def of[A, B] = new {
        def[F[+_]: TC] (c: T[A]) walkLz(f: A => Eval[F[B]]) : Eval[F[T[B]]] = c.walkLzA(f)
        override def[F[+_]: TC] (c: T[A]) walk(f: A => F[B]) : F[T[B]] = c.walkA(f)
    }

