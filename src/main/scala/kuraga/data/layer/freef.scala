package data.layer
import scala.annotation.tailrec
import EvalF.{ReadF, ErrorF, WriteF, ReadEvF, WriteEvF}



trait EvalF[-I[+_], +O[+_]] {
  def now[A](a: A): O[A]
  def defer[A](la: => I[A]): O[A]
  def flatMap[A, B](fa: I[A], f: A => I[B]): O[B]
}

type Ev[+a] = Layer1[EvalF, a]
type ReadL[-R, +a] = Layer1[ReadF[R], a]
type WriteL[+W, +a] = Layer1[WriteF[W], a]
type ErrorL[+E, +a] = Layer1[ErrorF[E], a]
type ReadEv[-R] = [+a] =>> Layer1[ReadEvF[R], a]
type WriteEv[+W, +a] = Layer1[WriteEvF[W], a]

type FK1 = [-i[+_], +o[+_]] =>> Any

trait Layer1[-P <: FK1, +A] {
  def unpack[R[+_]](p: P[[+a] =>> Layer1[P, a] , R]): R[A]
}


object EvalF  {  

  def now[A](a: A): Ev[A] = new Ev[A]{
    def unpack[R[+_]](p: EvalF[Ev, R]): R[A] = p.now(a)
  }

  def delay[A](la: => A): Ev[A] = now(la).defer

  def read[R]: Layer1[ReadF[R], R] = new Layer1{
    def unpack[X[+_]](p: ReadF[R][[+a] =>> Layer1[ReadF[R], a], X]) = p.read
  }

  final case class Done[+A](a: A) extends Ev[A]{
    def unpack[X[+_]](p: EvalF[Ev, X]): X[A] = p.now(a)
  }

  type L1[P <: FK1] = [+a] =>> Layer1[P, a]


  sealed trait Interp[P <: FK1, A]{
    def interpret(step: P[L1[P], L1[P]] ): Layer1[P, A]
  }

  final case class K[A,  P <: EvalF, B](fa : Layer1[P, A], cont: A => Layer1[P, B]) extends Layer1[P, B] with Interp[P, B] {
    def unpack[X[+_]](p: P[L1[P], X]): X[B] = p.flatMap[A, B](fa, cont)
    def interpret(step: P[L1[P], L1[P]] ): Layer1[P, B] = 
      fa.unpack(step) match {
        case Done(b)  => cont(b)
        case K(fx, f) => fx.flatMap(x => f(x).flatMap(cont))
        case ev       => ev.flatMap(cont)
      }
  }

  given {    
    def (self: Layer1[P, A]) flatMap[A, B, P <: EvalF ] (f: A => Layer1[P, B]) = new Layer1[P, B]{
      def unpack[R[+_]](p: P[L1[P], R]): R[B] = p.flatMap(self, f)
    }

    def (self: Layer1[P, A]) map[A, B, P <: EvalF](f: A => B): Layer1[P, B] = self.flatMap(a => EvalF.now(f(a)))
    
    def (self: => Layer1[P, A]) defer[A, P <: EvalF]: Layer1[P, A] = new Layer1[P, A]{
      def unpack[R[+_]](p: P[L1[P], R]): R[A] = p.defer(self)
    }

    def (self: Layer1[P, X]) run[P <: EvalF, X](step: P[L1[P], L1[P]]): X = {
        @tailrec def go(cur: Layer1[P, X]): X = cur.unpack(step) match{
            case Done(res)       => res    
            case k: K[_, P, X]   => go(k.interpret(step))        
            case next            => go(next)
        }

        go(self)
    }

    def (self: Ev[X]) value[X]: X = self.run[EvalF, X](EvalInterpreter[EvalF]())
    def (self: ReadEv[R][X]) runReader[R, X](r: R): X = self.run[ReadEvF[R], X](new EvalInterpreter[ReadEvF[R]] with ReaderInterpreter[ReadEvF[R], R](r))   
  }  

  class EvalInterpreter[P <: EvalF] extends EvalF[L1[P], L1[P]]{
    def now[A](a: A) = Done(a)
    def defer[A](la: => Layer1[P, A]) = la
    def flatMap[A, B](fa: Layer1[P, A], f: A => Layer1[P, B]) = K(fa, f) 
  }

  trait ReaderInterpreter[P <: EvalF, R](r: R) extends ReadF[R][L1[P], L1[P]]{
    def read = Done(r)
  }

  type ErrorF[-E] = [-i[+_], +o[+_]] =>> Raise[E, o] 
  type ReadF[+R] = [-i[+_], +o[+_]] =>> Read[R, o] 
  type WriteF[-W] = [-i[+_], +o[+_]] =>> Write[W, o] 
  type ReadEvF[+R] = [-i[+_], +o[+_]] =>> EvalF[i, o] & Read[R, o] 
  type WriteEvF[-W] = [-i[+_], +o[+_]] =>> EvalF[i, o] & Write[W, o] 

  trait Raise[-E, +O[+_]]{
    def raise[A](e: E): O[A]
  }

  trait Read[+R,  +O[+_]]{
    def read: O[R]
  }

  trait Write[-W, +O[+_]]{
    def write(w: W): O[Any]
  }
}



object EvalTest{
    given {
        def (xs: List[A]) foldr[A, B, P <: EvalF](f: (A, Layer1[P, B]) => Layer1[P, B])(lb: Layer1[P, B]): Layer1[P, B] = 
            xs match {
                case Nil => lb
                case head :: tail => f(head, foldr(tail)(f)(lb)).defer
            }

        def (xs: List[Long]) lsum[P <: EvalF]: Layer1[P, Long] = xs.foldr[Long, Long, P]((x, i) => i.map(_ + x))(EvalF.now(0L))
        def (xs: List[Long]) lsump[P <: ReadEvF[Long]]: Layer1[P, Long] = 
          xs.foldr[Long, Long, P]((x, i) => i.flatMap( s => EvalF.read[Long].map( p => s + x * p)))(EvalF.now(0L))
    }  
    
    def main(args: Array[String]): Unit = {
        println(List.range(1L, 1000000L).lsum.value)
        println(List.range(1L, 1000000L).lsump.runReader(7))
    }
}




