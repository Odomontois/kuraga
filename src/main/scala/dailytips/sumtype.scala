package dailytips.sumtype

import dailytips.sumtype.Sum.{AbsurdException, InjectR}

import scala.compiletime.ops.int.*
import scala.reflect.TypeTest

type Elem[T <: Tuple, N <: Int] = T match
  case x *: rest =>
    N match
      case S[m] => Elem[T, m]
      case 0    => x

class Sum[T <: Tuple](val depth: Int)(val elem: Elem[T, depth.type]):
  def absurd[empty >: T <: EmptyTuple]: Nothing = throw new AbsurdException

  def injectR[A]: Sum[A *: T] =
    val depth1 = depth + 1
    val elem1  = elem.asInstanceOf[Elem[A *: T, depth1.type]]
    Sum[A *: T](depth1)(elem1)

object Sum:
  type CNil = Sum[EmptyTuple]

  opaque type InjectL[A, T <: Tuple] <: Sum[A *: T] = Sum[A *: T]

  opaque type InjectR[A, T <: Tuple] <: Sum[A *: T] = Sum[A *: T]

  given [A, T <: Tuple]: TypeTest[Sum[A *: T], InjectL[A, T]] with
    def unapply(x: Sum[A *: T]) = if (x.depth == 0) Some(x) else None

  given [A, T <: Tuple]: TypeTest[Sum[A *: T], InjectR[A, T]] with
    def unapply(x: Sum[A *: T]) = if (x.depth > 0) Some(x) else None

//  def InjectL[A, T <: Tuple](a: A): InjectL[A, T] = Sum(0)(a)

  def InjectR[A, T <: Tuple](t: Sum[T]): InjectR[A, T] = t.injectR

  class AbsurdException extends RuntimeException
end Sum
