package playground.data.kayley

import cats.{Applicative, Functor}
import cats.syntax.all.given

type Cailey[F[+_], A] = Cailey.T[F, A]

object Cailey:
  private type Rep[F[+_], +A]           = [B] => F[B] => F[(A, B)]
  opaque type T[F[+_], +A] <: Rep[F, A] = Rep[F, A]

  extension [F[+_], A](fa: Cailey[F, A]) def rep(using F: Applicative[F]): F[A] = fa(F.unit).map(_._1)

  def apply[F[+_]: Applicative, A](fa: F[A]): Cailey[F, A] =
    [B] => (fb: F[B]) => (fa, fb).tupled

  given [F[+_]: Functor]: Applicative[Cailey[F, *]] with
    def pure[A](a: A) = [B] => (fb: F[B]) => fb.tupleLeft(a)

    def ap[A, B](fs: T[F, A => B])(xs: T[F, A]) =
      [C] => (fb: F[C]) => fs(xs(fb)).map { case (f, (x, xs)) => (f(x), xs) }

@main def test() =
  val x = Cailey(Option(1))
  val y = Cailey(Option(true))
  println((x, y).tupled.rep)
