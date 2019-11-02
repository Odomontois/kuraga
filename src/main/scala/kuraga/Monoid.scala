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
