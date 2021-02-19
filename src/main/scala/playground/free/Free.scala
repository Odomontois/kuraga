package playground.free
import cats.*
import scala.annotation.tailrec

enum Free[+F[_], A]:
  case Pure[A](a: A) extends Free[Void, A]
  case Bind[+F[_], B, A](fa: F[B], k: B => Free[F, A]) extends Free[F, A] 

  def flatMap[B, G[x] >: F[x]](k: A => Free[G, B]): Free[G, B] = this match 
    case Free.Pure(a) => k(a)
    case Free.Bind(fb, k1) => Free.Bind(fb, b => k1(b).flatMap(k))
  
  def map[B](f: A => B): Free[F, B] = flatMap(a => Free.Pure(f(a)))

  def semiFlatMap[B, G[x] >: F[x]](k: A => G[B]): Free[G, B] = flatMap(a => Free.suspend(k(a))) 

object Free:
  val unit: Free[Void, Unit]  = Pure(())

  def apply[A](a: => A): Free[Void, A] = unit.flatMap(_ => Pure(a))
  def suspend[F[_], A](fa: F[A]): Free[F, A] = Bind(fa, x => Pure(x))
  
  extension [F[_], A] (free: Free[F, A]) 
    def go (f: F[Free[F, A]] => Free[F, A]) (using F: Functor[F]): A = 
     free match
       case Pure(a) => a
       case Bind(fa, k) => f(F.map(fa)(k)).go(f)
    
    def runTailRec(using F: Monad[F]): F[A] = 
      F.tailRecM[Free[F, A], A](free){
        case Pure(a) => F.pure(Right(a))
        case Bind(fb, k) => F.map(fb)(a => Left(k(a)))
      }
  
  extension [F[_], G[_], A] (free: Free[F, A]) 
    def foldRun(f: F ~> G) (using G: Monad[G]): G[A] = 
      G.tailRecM(free){
        case Pure(a)     => G.pure(Right(a))
        case Bind(fb, k) => G.map(f(fb))(a => Left(k(a)))
      }

trait Run[F[_]]:
  self =>
  extension [A] (layer: F[A])  def run : Free[F, A]

  extension [A] (layer: F[Free[F, A]]) def step: Free[F, A] = layer.run.flatMap(identity)

  def ||[G[_]: RunOr]: Run[F || G] = new Run[F || G]{
    given Run[F] = self
    extension [A] (layer: F[A] | G[A]) def run : Free[F || G, A] = summon[RunOr[G]].runOr(layer)
  }

object Run:
  given Run[Void] with
    extension [A] (layer: Nothing) def  run: Free[Void, A] = Free.suspend(layer)

trait RunOr[F[_]] extends Run[F]:
  extension [G[_], A] (layer: F[A] | G[A]) def runOr(using Run[G]): Free[F || G, A]
  
  extension [A] (layer: F[A])  override def run: Free[F, A] = runOr[Void, A](layer)
