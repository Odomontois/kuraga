package kuraga
import scala.annotation.tailrec

enum Eval[+A]:
    case Pure(a: A)
    case FlatMap[A, +B](e: Eval[A], f: A => Eval[B]) extends Eval[B]

    def flatMap[B](f: A => Eval[B]): Eval[B] = FlatMap(this, f)
    def map[B](f: A => B): Eval[B] = flatMap(a => Pure(f(a)))

    @tailrec final def value : A = this match 
        case Pure(a)                   => a
        case FlatMap(Pure(x), f)       => f(x).value
        case FlatMap(FlatMap(x, f), g) => x.flatMap(y => f(y).flatMap(g)).value

object Eval:
    val unit: Eval[Unit] =  Pure(())
    def now[A](a: A): Eval[A] = Pure(a)
    
    def[A] (a: => A) delay: Eval[A]       = FlatMap(unit, _ => a.pure)
    def[A] (a: => Eval[A]) defer: Eval[A] = FlatMap(unit, _ => a)
    
    given StackSafeMonad[Eval]:
        def[A] (a: A) pure : Eval[A] = Pure(a)
        def[A, B] (a: Eval[A]) flatMap (f: A => Eval[B]): Eval[B] = FlatMap(a, f)