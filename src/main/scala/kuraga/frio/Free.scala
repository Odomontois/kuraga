package kuraga.frio
import cats._

enum Free[+F[_], +A]{
  case Pure(a: A)
  case Suspend(fa: F[A])
  case Bind[+F[_], B, +A](fa: Free[F, B], k: B => Free[F, A]) extends Free[F, A]

  def flatMap[B, G[x] >: F[x]](k: A => Free[G, B]): Free[G, B] = Bind(this, k)
  def map[B](f: A => B): Free[F, B] = flatMap(a => Pure(f(a)))
  def semiFlatMap[B, G[x] >: F[x]](k: A => G[B]): Free[G, B] = Bind(this, a => Suspend(k(a))) 
}

object Free{
  implied {
    def (free: Free[F, A]) runTailRec[F[_], A] given (F: Monad[F]): F[A] = 
      F.tailRecM(free){
        case Pure(a) => F.pure(Right(a))
        case Suspend(fa) => F.map(fa)(Right(_))
        case Bind(fr, k) => fr match {
          case Pure(b) => F.pure(Left(k(b)))
          case Suspend(fb) => F.map(fb)(b => Left(k(b)))
          case Bind(fr1, k1) => F.pure(Left(Bind(fr1, c => Bind(k1(c), k))))
        }
      }

    def (free: Free[F, A]) foldRun[F[_], G[_], A] (f: F ~> G) given (G: Monad[G]): G[A] = 
      G.tailRecM(free){
        case Pure(a) => G.pure(Right(a))
        case Suspend(fa) => G.map(f(fa))(Right(_))
        case Bind(fr, k) => G.map(fr.foldRun(f))(b => Left(k(b)))
      }
  }
}

