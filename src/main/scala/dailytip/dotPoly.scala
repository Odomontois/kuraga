// Daily scala 3 tip #2

// Polymorphic function syntax is still very cumbersome,
// but often you can use a dependent function instead
// https://dotty.epfl.ch/docs/reference/new-types/dependent-function-types.html
// or a SAM-type with a dependently typed method

package dailytip.dotPoly

import cats.~>
import cats.data.OneAnd
import cats.free.Free

trait Holder:
  type T

trait FunK[F[_], G[_]] extends (F ~> G):
  def applyDep(tp: Holder): F[tp.T] => G[tp.T]

  def apply[A](fa: F[A]): G[A] = applyDep(new { type T = A })(fa)
end FunK

extension [F[_], A](fa: Free[F, A])
  def kmap[G[_]](f: FunK[F, G]): Free[G, A] = fa.mapK(f)


//@main def foo() =
//  println(OneAnd(1, List(1, 2, 3)).mapK(FunK(_ => _.headOption)))
//  println(OneAnd(1, Nil).mapK(FunK(_ => _.toVector)))
