package kuraga.free
import cats._
import scala.annotation.tailrec

enum Free[+F[_], A]{
  case Pure[A](a: A) extends Free[Void, A]
  case Bind[+F[_], B, A](fa: F[B], k: B => Free[F, A]) extends Free[F, A] 

  def flatMap[B, G[x] >: F[x]](k: A => Free[G, B]): Free[G, B] = this match {
      case Free.Pure(a) => k(a)
      case Free.Bind(fb, k1) => Free.Bind(fb, b => k1(b).flatMap(k))
  }
  def map[B](f: A => B): Free[F, B] = flatMap(a => Free.Pure(f(a)))

  def semiFlatMap[B, G[x] >: F[x]](k: A => G[B]): Free[G, B] = flatMap(a => Free.suspend(k(a))) 
}


object Free{
  val unit: Free[Void, Unit]  = Pure(())

  def apply[A](a: => A): Free[Void, A] = unit flatMap (_ => Pure(a))
  def suspend[F[_], A](fa: F[A]): Free[F, A] = Bind(fa, x => Pure(x))
  
  given {
    @tailrec
    def (free: Free[F, A]) go[F[+_], A] (f: F[Free[F, A]] => Free[F, A]) given (F: Functor[F]): A = 
     free match {
       case Pure(a) => a
       case Bind(fa, k) => f(F.map(fa)(k)) go f
     }

    
    def (free: Free[F, A]) runTailRec[F[_], A] given (F: Monad[F]): F[A] = 
      F.tailRecM[Free[F, A], A](free){
        case Pure(a) => F.pure(Right(a))
        case Bind(fb, k) => F.map(fb)(a => Left(k(a)))
      }

    def (free: Free[F, A]) foldRun[F[_], G[_], A] (f: F ~> G) given (G: Monad[G]): G[A] = 
      G.tailRecM(free){
        case Pure(a)     => G.pure(Right(a))
        case Bind(fb, k) => G.map(f(fb))(a => Left(k(a)))
      }
  }
}

trait Run[F[_]]{ self =>
  def (layer: F[A]) run[A] : Free[F, A]

  def (layer: F[Free[F, A]]) step[A]: Free[F, A] = layer.run flatMap identity

  def ||[G[_]: RunOr]: Run[F || G] = new Run[F || G]{
    delegate for Run[F] = self
    def (layer: F[A] | G[A]) run[A]: Free[F || G, A] = layer.runOr[F, A]
  }
}

object Run {
  delegate for Run[Void]{
    def (layer: Nothing) run[A]: Free[Void, A] = Free.suspend(layer)
  }
}

trait RunOr[F[_]] extends Run[F]{
  def (layer: F[A] | G[A]) runOr[G[_], A] given Run[G]: Free[F || G, A]
  
  override def (layer: F[A]) run[A]: Free[F, A] = layer.runOr[Void, A]
}