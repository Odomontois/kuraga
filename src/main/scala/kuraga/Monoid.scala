package kuraga
import Eval.defer
import scala.annotation.tailrec
import kuraga.Apply

trait Semigroup[T]:
  extension (x: T) infix def combine(y: T): T
  extension (x: T) infix def |+|(y: T): T                   = x.combine(y)
  extension (x: Eval[T]) def combineLz(y: Eval[T]): Eval[T] = x.map2(y)(_ combine _)

object Semigroup:

  opaque type Sum[L] >: L = L
  given [L](using L: Numeric[L]): Monoid[Sum[L]] with
    def default                        = L.zero
    extension (x: L) def combine(y: L) = L.plus(x, y)
trait Default[T]:
  def default: T

trait Monoid[T] extends Semigroup[T] with Default[T]

object Monoid:
  def default[A](using M: Monoid[A]): A = M.default

