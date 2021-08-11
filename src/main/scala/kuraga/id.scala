package kuraga
import scala.quoted.*
import scala.annotation.tailrec

opaque type Id[+A] = A

object Id:
  def apply[A](x: A): Id[A]          = x
  extension [A](i: Id[A]) def get: A = i

  given Monad[Id] with
    def pure[A](a: A)                                        = a
    extension [A, B](a: Id[A]) def flatMap(f: A => Id[B])    = f(a)
    extension [A, B](a: A)
      @tailrec def tailRecM(f: A => Id[Either[A, B]]): Id[B] =
        f(a) match
          case Left(a1) => a.tailRecM(f)
          case Right(b) => b
  given Reducible[Id] with
    extension [A, B](c: Id[A]) def reduceMapA(f: A => Eval[B])(using Semigroup[B]): Eval[B] = f(c.get)
