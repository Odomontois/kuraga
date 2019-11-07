package playground.traits

import kuraga.{Monad, Id}

trait EitherImpl[F[_], E] (given F: Monad[F]) extends Monad[[A] =>> F[Either[E, A]]]
    def [A](a: A) pure: F[Either[E, A]] = F.pure(Right(a)) 
    def [A, B](fa: F[Either[E, A]]) flatMap (f: A => F[Either[E, B]]): F[Either[E, B]] = 
        F.flatMap(fa) {
            case Left(e) => F.pure(Left(e))
            case Right(a) => f(a)
        }
    def [A, B] (a: A) tailRecM (f: A => F[Either[E, Either[A, B]]]): F[Either[E, B]] =
        F.tailRecM(a)( a => F.map(f(a)){
                case Left(e)         => Right(Left(e))
                case Right(Left(a))  => Left(a)
                case Right(Right(b)) => Right(Right(b))
            }) 


object Kek extends EitherImpl[Id, String]
