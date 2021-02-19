package kuraga
import Eval.defer
import scala.annotation.tailrec
import kuraga.Apply

trait Semigroup[T]:
    extension (x: T) infix def combine(y: T): T
    extension (x: Eval[T]) def combineLz (y: Eval[T]) : Eval[T] = x.map2(y)(_ combine _)

trait Default[T]:
    def default: T

trait Monoid[T] extends Semigroup[T] with Default[T]