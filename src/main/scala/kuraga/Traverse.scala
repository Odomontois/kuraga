package kuraga
import Eval.delay

trait Walk[S, T, A, B, -TC[_[_]]]:
    def walkLz[F[+_]: TC] (c: S) (f: A => Eval[F[B]]): Eval[F[T]]

    def walk[F[+_]: TC] (c: S) (f: A => F[B]): F[T] = 
       walkLz(c)(a => f(a).delay).value



trait Walkable[T[_], A, TC[_[_]]] extends Forall2[[A, B] =>> Walk[T[A], T[B], A, B, TC]]:
    self =>
    def walkLzA [F[_]: TC, A, B] (fa: T[A]) (f: A => Eval[F[B]])  : Eval[F[T[B]]]

    def walkA[F[+_]: TC, A, B] (c: T[A])(f: A => F[B]) : F[T[B]] = 
        walkLzA(c)(a => f(a).delay).value

    def of[A, B] = new {
        def walkLz[F[+_]: TC] (c: T[A]) (f: A => Eval[F[B]]) : Eval[F[T[B]]] = walkLzA(c)(f)
        override def walk[F[+_]: TC] (c: T[A]) (f: A => F[B]) : F[T[B]] = walkA(c)(f)
    }

