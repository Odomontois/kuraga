package kuraga
import Eval.defer
import scala.annotation.tailrec

trait Semigroup[T]
    def (x: T) combine(y: T): T
    final def (x: T) |+| (y: T) = x combine y
    def (x: Eval[T]) combineLz (y: Eval[T]) : Eval[T] = x.map2(y)(_ combine _)



trait Default[T]
    def default: T

trait Monoid[T] extends Semigroup[T] with Default[T]

opaque type Endo[A] = Eval[A] => Eval[A]
object Endo
    def apply[A](f: Eval[A] => Eval[A]): Endo[A] = f
    given: {
        def [A](endo: Endo[A]) apply (ea: Eval[A]) : Eval[A] = endo(ea)
    }
    given [A] : Monoid[Endo[A]]
        def default = x => x
        def (x: Endo[A]) combine (y: Endo[A]) = e => x(y(e)).defer
        override def (lx: Eval[Endo[A]]) combineLz (ly: Eval[Endo[A]]) = 
            Eval.now(Endo(la => lx.flatMap(_(ly.flatMap(_(la))))))

case class Compose[A, B, C](f: A => B, g: B => C) extends (A => C)
    @tailrec final def apply(a: A): C = f match
        case Compose(u, v) => Compose(u, Compose(v, g))(a) 
        case f => g(f(a))