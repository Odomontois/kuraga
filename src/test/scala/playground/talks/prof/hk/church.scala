package playground.talks.prof.hk.church

trait Bools[F[_]]:
  def True: F[Boolean]
  def False: F[Boolean]
  def Not(x: F[Boolean]): F[Boolean]
  def And(x: F[Boolean], y: F[Boolean]): F[Boolean]
  def Or(x: F[Boolean], y: F[Boolean]): F[Boolean]

trait Numbers[F[_]]:
  def fromInt(x: Int): F[Int]
  def plus(x: F[Int], y: F[Int]): F[Int]
  def multiply(x: F[Int], y: F[Int]): F[Int]

trait Comparison[F[_]]:
  def less(x: F[Int], y: F[Int]): F[Boolean]
  def equals(x: F[Int], y: F[Int]): F[Boolean]

trait Repr[Alg[f[_]], A]:
  def apply[F[_]](alg: Alg[F]): F[A]
