package dailytips.dependent

import cats.~>
import cats.free.Free



type Pair[A]    = (A, A)
type BinTree[A] = Free[Pair, A]

val binTree: BinTree[Int] = Free.roll((Free.pure(1), Free.liftF((2, 3))))
@main def foo() =
  println(binTree.foldMap(depK(_ => (a, b) => List(a, b))))
  val f = [A] => (p: Pair[A]) => p match { case (a, b) => List(a, b) }
  println(binTree.foldMap(polyK(f)))
end foo


def depK[F[_], G[_]](f: (o: { type T }) => F[o.T] => G[o.T]): F ~> G =
  new:
    def apply[A](fa: F[A]) = f(new { type T = A }: { type T = A })(fa)

def polyK[F[_], G[_]](f: [A] => F[A] => G[A]): F ~> G =
  new:
    def apply[A](fa: F[A]) = f(fa)


