package kuraga
package free
import cats._
import scala.annotation.tailrec


enum Free[+F[_], +A]{
  case Pure(a: A)
  case Suspend(fa: F[A])
  case Bind[+F[_], B, +A](fa: Free[F, B], k: B => Free[F, A]) extends Free[F, A]

  def flatMap[B, G[x] >: F[x]](k: A => Free[G, B]): Free[G, B] = Bind(this, k)
  def map[B](f: A => B): Free[F, B] = flatMap(a => Pure(f(a)))
  def semiFlatMap[B, G[x] >: F[x]](k: A => G[B]): Free[G, B] = Bind(this, a => Suspend(k(a))) 
}

object Free{
  val unit: Free[Void, Unit]  = Pure(())

  def apply[A](a: => A): Free[Void, A] = unit flatMap (_ => Pure(a))
  
  implied {
    @tailrec
    def (free: Free[F, A]) go[F[_], A] (f: F[Free[F, A]] => Free[F, A]) given (F: Functor[F]): A = 
     free match {
       case Pure(a) => a
       case Suspend(fa) => f(F.map(fa)(Pure(_))) go f
       case Bind(fa, k) => fa match {
         case Pure(b) => k(b) go f
         case Suspend(fb) => f(F.map(fb)(k)) go f
         case Bind(fr1, k1) => Bind(fr1, c => Bind(k1(c), k)) go f
       }
     }
    
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
        case Pure(a)     => G.pure(Right(a))
        case Suspend(fa) => G.map(f(fa))(Right(_))
        case Bind(fr, k) => G.map(fr.foldRun(f))(b => Left(k(b)))
      }
  }
}

trait Run[F[_]]{ self =>
  def (layer: F[A]) run[A] : Free[F, A]

  def (layer: F[Free[F, A]]) step[A]: Free[F, A] = layer.run flatMap identity

  def (free: Free[F, A]) runFree[A] given Functor[F]: A = free go (_.step)

  def ||[G[_]: RunOr]: Run[F || G] = new Run[F || G]{
    implied for Run[F] = self
    def (layer: F[A] | G[A]) run[A]: Free[F || G, A] = layer.runOr[F, A]
  }
}

object Run {
  implied for Run[Void]{
    def (layer: Nothing) run[A]: Free[Void, A] = Free.Suspend(layer)
  }
}

trait RunOr[F[_]] extends Run[F]{
  def (layer: F[A] | G[A]) runOr[G[_], A] given Run[G]: Free[F || G, A]
  
  override def (layer: F[A]) run[A]: Free[F, A] = layer.runOr[Void, A]
}