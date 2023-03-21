import scala.util.control.TailCalls
import scala.util.control.TailCalls.*

List(1, 2, 3).show

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def lmap[B](f: A => TailRec[B]): TailRec[F[B]]
    def map[B](f: A => B): F[B] = lmap(f andThen done).result

given Functor[List] with
  extension [A](fa: List[A])
    def lmap[B](f: A => TailRec[B]): TailRec[List[B]] =
      tailcall(
        fa.foldRight(TailCalls.done(List.empty[B])) { (a, acc) =>
          for
            h <- f(a)
            t <- acc
          yield h :: t
        }
      )
end given

trait Show[A]:
  extension (x: A)
    def lshow: TailRec[String]
    def show: String = x.lshow.result

given [A: Show]: Show[List[A]] = _.lmap(_.lshow).map(_.mkString("[", ", ", "]"))

given Show[Int] = x => done(x.toString)

case class Fix[+F[+_]](unfix: F[Fix[F]]):
  def mapKL[A, G[+_]: Functor](f: [A] => F[A] => TailRec[G[A]]): TailRec[Fix[G]] =
    for
      top  <- f(unfix)
      rest <- top.lmap(_.mapKL(f))
    yield Fix(rest)

  def mapK[A, G[+_]: Functor](f: [A] => F[A] => G[A]): Fix[G] = mapKL([A] => (fa: F[A]) => done(f(fa))).result
end Fix

given [F[+_]](using go: Show[Fix[F]] ?=> Show[F[Fix[F]]]): Show[Fix[F]] =
  given fixed: Show[F[Fix[F]]] = go
  _.unfix.lshow

def nat(x: Int): Fix[Option] =
  def go(x: Int, acc: Fix[Option]): Fix[Option] =
    if x == 0 then acc else go(x - 1, Fix(Some(acc)))
  go(x, Fix(None))

nat(100000).mapK([A] => (_: Option[A]).toList).show.length
// Fix(List(Fix(Nil), Fix(List(Fix(Nil), Fix(Nil))), Fix(List(Fix(List(Fix(Nil))))))).show
