package kuraga
import scala.quoted._
import scala.annotation.tailrec

opaque type Id[+A] = A

object Id
    def apply[A](x: A): Id[A] = x
    given [A] (i: Id[A]) extended with
        def  get: A = i
    

    given Monad[Id]
        def [A] (a: A) pure = a
        def [A, B] (a: Id[A]) flatMap (f: A => Id[B]) = f(a)
        @tailrec def [A, B] (a: A) tailRecM (f: A => Id[Either[A, B]]) : Id[B] = 
            f(a) match
                case Left(a1) => a.tailRecM(f)
                case Right(b) => b
    given Reducible[Id]
        def [A, B] (c: Id[A]) reduceMap (f: A => Eval[B])(given Semigroup[B]): Eval[B] = f(c.get)





