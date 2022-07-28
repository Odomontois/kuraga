package playground.talks.prof.hk.fix

import kuraga.Traverse

trait TraverseHK[Alg[f[_], _]]:
  def traverse[A, F[_], X[_], Y[_]](alg: Alg[X, A])(f: [a] => X[a] => F[Y[a]]): Alg[Y, A]

case class Fix[+Alg[+_[_], _], R](alg: Alg[Fix[Alg, *], R])

enum Bools[+F[_], A] derives TraverseHK:
  case True                                    extends Bools[Nothing, Boolean]
  case False                                   extends Bools[Nothing, Boolean]
  case Not[F[_]](x: F[Boolean])                extends Bools[F, Boolean]
  case And[F[_]](x: F[Boolean], y: F[Boolean]) extends Bools[F, Boolean]
  case Or[F[_]](x: F[Boolean], y: F[Boolean])  extends Bools[F, Boolean]

enum Numbers[+F[_], A] derives TraverseHK:
  case FromInt(x: Int)                extends Numbers[Nothing, Int]
  case Plus(x: F[Int], y: F[Int])     extends Numbers[Nothing, Int]
  case Multiply(x: F[Int], y: F[Int]) extends Numbers[Nothing, Int]

enum Comparison[+F[_], A] derives TraverseHK:
  case Less(x: F[Int], y: F[Int])   extends Comparison[F, Boolean]
  case Equals(x: F[Int], y: F[Int]) extends Comparison[F, Boolean]


object TraverseHK:
  inline def derived[Alg[f[_], _]]: TraverseHK[Alg] = ???
