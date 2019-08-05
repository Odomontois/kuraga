package data.layer
import scala.annotation.tailrec

trait EvalF[-I[+_], +O[+_]] {
  def now[A](a: A): O[A]
  def defer[A](la: => I[A]): O[A]
  def flatMap[A, B](fa: I[A], f: A => I[B]): O[B]
}

type Eval[+a] = Layer1[EvalF, a]

object EvalF extends EvalF[Eval, Eval] {
  def now[A](a: A): Eval[A] = new Eval[A]{
    def unpack[R[+_]](p: EvalF[Eval, R]): R[A] = p.now(a)
  }
  def defer[A](la: => Eval[A]): Eval[A] = new Eval[A]{
    def unpack[R[+_]](p: EvalF[Eval, R]): R[A] = p.defer(la)
  }
  def flatMap[A, B](fa: Eval[A], f: A => Eval[B]): Eval[B] = new Eval[B]{
    def unpack[R[+_]](p: EvalF[Eval, R]): R[B] = p.flatMap[A, B](fa, f)
  }

  def delay[A](la: => A): Eval[A] = defer(now(la))

  given {    
    def (self: Eval[A]) flatMap[A, B] (f: A => Eval[B]) = EvalF.flatMap(self, f)
    def (self: Eval[A]) map[A, B](f: A => B) = self.flatMap(a => now(f(a)))

    def (self: Eval[A]) value[A]: A = {
        def stepFM[b] = new EvalF[Eval , [+a] =>> (a => Eval[b]) => Eval[b]]{
            def now[A](a: A) = f => f(a)
            def defer[A](la: => Eval[A]) = f => EvalF.flatMap(la, f)
            def flatMap[A, B](fa: Eval[A], f: A => Eval[B]) = g => fa.flatMap(a => f(a).flatMap(g))
        }

        val step = new EvalF[Eval, [+a] =>> Either[Eval[a], a]]{
            def now[A](a: A) = Right(a)
            def defer[A](la: => Eval[A]) = Left(la)
            def flatMap[A, B](fa: Eval[A], f: A => Eval[B]) = Left(fa.unpack(stepFM)(f))    
        }

        @tailrec def go(cur: Eval[A]): A = cur.unpack(step) match{
            case Left(next) => go(next)
            case Right(res) => res
        }

        go(self)
    }
  }
}

trait Layer1[-P[-i[+_], +o[+_]], +A] {
  def unpack[R[+_]](p: P[[+a] =>> Layer1[P, a] , R]): R[A]
}

object EvalTest{
    given {
        def (xs: List[A]) foldr[A, B](f: (A, Eval[B]) => Eval[B])(lb: Eval[B]): Eval[B] = 
            xs match {
                case Nil => lb
                case head :: tail => EvalF.defer(f(head, foldr(tail)(f)(lb)))
            }

        def (xs: List[Long]) lsum: Long = xs.foldr[Long, Long]((x, i) => i.map(_ + x))(EvalF.now(0L)).value
    }  
    
    def main(args: Array[String]): Unit = {
        println(List.range(1L, 1000000L).lsum)
    }
}
