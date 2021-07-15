package playground.traits

import kuraga.{Monad, Id}

// trait EitherImpl[F[_], E] (using F: Monad[F]) extends Monad[[A] =>> F[Either[E, A]]]:
//     extension [A] (a: A) def pure: F[Either[E, A]] = F.pure(Right(a))
//     extension [A, B] (fa: F[Either[E, A]]) def flatMap (f: A => F[Either[E, B]]): F[Either[E, B]] =
//         F.flatMap(fa) {
//             case Left(e) => F.pure(Left(e))
//             case Right(a) => f(a)
//         }
//     def [A, B] (a: A) tailRecM (f: A => F[Either[E, Either[A, B]]]): F[Either[E, B]] =
//         F.tailRecM(a)( a => F.map(f(a)){
//                 case Left(e)         => Right(Left(e))
//                 case Right(Left(a))  => Left(a)
//                 case Right(Right(b)) => Right(Right(b))
//             })

// object Kek extends EitherImpl[Id, String]

val zoneId = tryToGetSome(
  fromRequest = Some({ x => x.length }),
  default = 0
)

private def tryToGetSome[A](fromRequest: Option[String => A] = None, default: A): A = {
  default
}
