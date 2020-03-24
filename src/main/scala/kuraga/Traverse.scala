package kuraga

trait Walk[S, T, A, B, -TC[_[_]]]:
    def[F[+_]: TC] (c: S) walkLz(f: A => Eval[F[B]]) : Eval[F[T]]

    def[F[+_]: TC] (c: S) walk(f: A => F[B]) : F[T] = 
       c.walkLz(a => Eval.delay(f(a))).value



trait Walkable[T[_], A, TC[_[_]]] extends Forall2[[A, B] =>> Walk[T[A], T[B], A, B, TC]]:
    self =>
    def [F[_]: TC, A, B] (fa: T[A]) walkLz (f: A => Eval[F[B]])  : Eval[F[T[B]]]

    def[F[+_]: TC, A, B] (c: T[A]) walk(f: A => F[B]) : F[T[B]] = 
        c.walkLz(a => Eval.delay(f(a))).value

    def of[A, B] = new {
        def[F[+_]: TC] (c: T[A]) walkLz(f: A => Eval[F[B]]) : Eval[F[T[B]]] = self.walkLz(c)(f)
        override def[F[+_]: TC] (c: T[A]) walk(f: A => F[B]) : F[T[B]] = self.walk(c)(f)
    }

