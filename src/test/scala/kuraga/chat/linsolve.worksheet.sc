trait Functor[F[_]]:
    extension [A](fa: F[A]) def map[B](f: A => B): F[B]

case class LinEq[+A](xq: A, yq: A, r: A)

object LinEq:
    given Functor[LinEq] = ???

case class LinSys[+A](eq1: LinEq[A], eq2: LinEq[A])

object LinSys:
    given Functor[LinSys] = ???

case class Ratio[A](num: A, den: A)

object Ratio:
    def whole[A](x: A)(using A: Numeric[A]): Ratio[A] = Ratio(x, A.one)
    given [A: Integral]: Fractional[Ratio[A]] = ???

trait Solver[A, B]:
  extension (sys: LinSys[A]) def solve: Option[(B, B)]

object Solver:
    given [A: Fractional]: Solver[A, A] = ???
    given [A: Integral]: Solver[A, Ratio[A]] with 
        extension (sys: LinSys[A]) 
            def solve: Option[(Ratio[A], Ratio[A])] = 
                sys.map(Ratio.whole).solve
