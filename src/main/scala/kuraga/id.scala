package kuraga
import scala.quoted._
import scala.annotation.tailrec

opaque type Id[+A] = A

object Id:
    def apply[A](x: A): Id[A] = x
    extension  on[A] (i: Id[A]):
        def  get: A = i
    

    given Monad[Id]:
        def [A] (a: A) pure = a
        def [A, B] (a: Id[A]) flatMap (f: A => Id[B]) = f(a)
        @tailrec def [A, B] (a: A) tailRecM (f: A => Id[Either[A, B]]) : Id[B] = 
            f(a) match
                case Left(a1) => a.tailRecM(f)
                case Right(b) => b
    given Reducible[Id]:
        def [A, B] (c: Id[A]) reduceMap (f: A => Eval[B])(using Semigroup[B]): Eval[B] = f(c.get)





