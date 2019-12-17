package playground.data.layer

import scala.annotation.tailrec
import Effects._
import EvalF.given

type FK1 = [-i[+_], +o[+_]] =>> Any

trait Layer1[-P <: FK1, +A] {
  def unpack[R[+_]](p: P[[+a] =>> Layer1[P, a] , R]): R[A]
}


object EvalF  {  

  val unit : Pure[Unit] = Done(())

  def now[A](a: A): Ev[A] = new Ev[A]{
    def unpack[R[+_]](p: EvalF[Ev, R]): R[A] = p.now(a)
  }

  def delay[A](la: => A): Ev[A] = now(la).defer

  def read[R]: Layer1[ReadF[R], R] = new Layer1{
    def unpack[X[+_]](p: ReadF[R][L1[ReadF[R]], X]) = p.read
  }

  def write[W](w: W): Layer1[WriteF[W], Any] = new Layer1[WriteF[W], Any]{
    def unpack[X[+_]](p: WriteF[W][L1[WriteF[W]], X]): X[Any] = p.write(w)
  }

  final case class Done[+A](a: A) extends Pure[A]{
    def unpack[X[+_]](p: NowF[X]): X[A] = p.now(a)
  }

  final case class Raised[+E](e: E) extends ErrorL[E, Nothing]{
    def unpack[X[+_]](p: Raise[E, X]): X[Nothing] = p.raise(e) 
  } 

  type L1[P <: FK1] = [+a] =>> Layer1[P, a]



  final case class K[A,  P <: EvalF, B](fa : Layer1[P, A], cont: A => Layer1[P, B]) extends Layer1[P, B]  {
    def unpack[X[+_]](p: P[L1[P], X]): X[B] = p.flatMap[A, B](fa, cont)
  }
  
  private def [P <: EvalF, A, B] (k: K[A, P, B]) interpret(step: P[L1[P], L1[P]] ): Layer1[P, B] = k.fa.unpack(step) match {
        case Done(b)  => k.cont(b)
        case K(fx, f) => fx.flatMap(x => f(x).flatMap(k.cont))
        case ev       => ev.flatMap(k.cont)
      }

  private def [P <: ErrorEvF[E], A, B, E] (k : K[A, P, B]) interpretErr(step: P[L1[P], L1[P]]): Layer1[P, B] = 
    k.fa.unpack(step) match {
      case Done(b)     => k.cont(b)
      case e@Raised(_) => e
      case K(fx, f)    => fx.flatMap(x => f(x).flatMap(k.cont))
      case ev          => ev.flatMap(k.cont)
    }
  

  given [A, B, P <: EvalF ] (self: Layer1[P, A]) extended with  
    def flatMap (f: A => Layer1[P, B]) = new Layer1[P, B]{
      def unpack[R[+_]](p: P[L1[P], R]): R[B] = p.flatMap(self, f)
    }

    def map(f: A => B): Layer1[P, B] = self.flatMap(a => EvalF.now(f(a)))
  
  given [A, B, P <: EvalF ] (self: => Layer1[P, A]) extended with     
    def defer: Layer1[P, A] = new Layer1[P, A]{
      def unpack[R[+_]](p: P[L1[P], R]): R[A] = p.defer(self)
    }


  given [P <: EvalF, X] (self: Layer1[P, X]) extended with 
    def run(step: P[L1[P], L1[P]]): X = {
        @tailrec def go(cur: Layer1[P, X]): X = cur.unpack(step) match{
            case Done(res)       => res    
            case k: K[_, P, X]   => go(k.interpret(step))        
            case next            => go(next)
        }

        go(self)
    }

  
  given [P <: ErrorEvF[E], S, E, X](self: Layer1[P, X]) extended with 
    def exec(step: P[L1[P], L1[P]] & HasState[S]): (S, Either[E, X]) = {
        @tailrec def go(cur: Layer1[P, X]): (S, Either[E, X]) = cur.unpack(step) match{
            case Done(res)       => (step.state, Right(res))
            case Raised(err)     => (step.state, Left(err))  
            case k: K[_, P, X]   => go(k.interpretErr(step))        
            case next            => go(next)
        }

        go(self)
    }

  given [X](self: Ev[X]) extended with
    def value: X = self.run[EvalF, X](EvalInterpreter[EvalF]())

  given [R, X] (self: ReadEv[R][X]) extended with
    def runReader(r: R): X = self.run[ReadEvF[R], X](
      new EvalInterpreter[ReadEvF[R]] with ReaderInterpreter[ReadEvF[R], R](r))   

  
  given[S, E, X] (self: ExecEv[S, E, X]) extended with 
    def runStateE(init: S) : (S, Either[E, X]) = self.exec[ExecF[S, E], S, E, X](
      new ExecInterpreter[S, E, ExecF[S, E]](init)
    )
    

  class EvalInterpreter[P <: EvalF] extends EvalF[L1[P], L1[P]]{
    def now[A](a: A) = Done(a)
    def defer[A](la: => Layer1[P, A]) = la
    def flatMap[A, B](fa: Layer1[P, A], f: A => Layer1[P, B]) = K(fa, f) 
  }

  trait HasState[S]{
    def state : S
  }

  class ExecInterpreter[S, -E, P <: ExecF[S, E]](var state : S) extends EvalInterpreter[P] with Raise[E, L1[P]] with State[S, L1[P]] with HasState[S]{
    def raise(e : E) = Raised(e)
    def read = Done(state)
    def write(s : S) = {
      state = s
      unit
    }
  }

  trait ReaderInterpreter[P <: EvalF, R](r: R) extends ReadF[R][L1[P], L1[P]]{
    def read = Done(r)
  }
}

object Effects{
  type ErrorF[-E] = [-i[+_], +o[+_]] =>> Raise[E, o] 
  type ReadF[+R] = [-i[+_], +o[+_]] =>> Read[R, o] 
  type WriteF[-W] = [-i[+_], +o[+_]] =>> Write[W, o] 
  type ReadEvF[+R] = [-i[+_], +o[+_]] =>> EvalF[i, o] & Read[R, o] 
  type WriteEvF[-W] = [-i[+_], +o[+_]] =>> EvalF[i, o] & Write[W, o] 
  type ErrorEvF[-E] = [-i[+_], +o[+_]] =>> EvalF[i, o] &  Raise[E, o] 
  type StateF[S] = [-i[+_], +o[+_]] =>> EvalF[i, o] & State[S, o] 
  type ExecF[S, -E] = [-i[+_], +o[+_]] =>> EvalF[i, o] & Raise[E, o] & State[S, o]
  type SuspendF[-F[+_]] = [-i[+_], +o[+_]] =>> Suspend[F, o]
  type FreeF[-F[+_]] = [-i[+_], +o[+_]] =>> EvalF[i, o] & Suspend[F, o]

  trait Raise[-E, +O[+_]]{
    def raise(e: E): O[Nothing]
  }

  trait Read[+R,  +O[+_]]{
    def read: O[R]
  }

  trait Write[-W, +O[+_]]{
    def write(w: W): O[Any]
  }

  trait Suspend[-F[+_], +O[+_]]{
    def suspend[A](f: F[A]): O[A]
  }

  trait State[S, +O[+_]] extends Read[S, O] with Write[S, O]

  trait NowF[+O[+_]]{
    def now[A](a: A): O[A]
  }

  trait EvalF[-I[+_], +O[+_]] extends NowF[O]{ 
    def defer[A](la: => I[A]): O[A]
    def flatMap[A, B](fa: I[A], f: A => I[B]): O[B]
  }

  type NowFF[-I[+_], +O[+_]] = NowF[O]
  type Ev[+a] = Layer1[EvalF, a]
  type Pure[+a] = Layer1[NowFF, a]
  type ReadL[-R, +a] = Layer1[ReadF[R], a]
  type WriteL[+W, +a] = Layer1[WriteF[W], a]
  type ErrorL[+E, +a] = Layer1[ErrorF[E], a]
  type ReadEv[-R] = [+a] =>> Layer1[ReadEvF[R], a]
  type WriteEv[+W, +a] = Layer1[WriteEvF[W], a]
  type StateEv[S, +a] = Layer1[StateF[S], a]
  type ExecEv[S, E, +a] = Layer1[ExecF[S, E], a]
  type Free[+F[+_], +a] = Layer1[FreeF[F], a]
}



object EvalTest{

    def [A, B, P <: EvalF] (xs: List[A]) foldr(f: (A, Layer1[P, B]) => Layer1[P, B])(lb: Layer1[P, B]): Layer1[P, B] = 
        xs match {
            case Nil => lb
            case head :: tail => f(head, foldr(tail)(f)(lb)).defer
        }

    def (xs: List[Long]) lsum: Ev[Long] = xs.foldr[Long, Long, EvalF]((x, i) => i.map(_ + x))(EvalF.now(0L))
    def (xs: List[Long]) lsump: ReadEv[Long][Long] = 
      xs.foldr[Long, Long, ReadEvF[Long]]{(x, i) =>
        i.flatMap(s => EvalF.read[Long].map( p => s + x * p))
    }(EvalF.now(0L))

    def (xs: List[Long]) lsumAndCount: StateEv[Long, Long] = 
      xs.foldr[Long, Long, StateF[Long]]((x, i) => 
        for{
          s <- i
          q <- EvalF.read[Long]
          _ <- EvalF.write(q + 1)
        } yield s + x
      )(EvalF.now(0L))
      
    
    def main(args: Array[String]): Unit = {
      val lst =List.range(1L, 1000001L) 
        println(lst.lsum.value)
        println(lst.lsump.runReader(7))
        println(lst.lsumAndCount.runStateE[Long, Nothing, Long](0))
    }
}




