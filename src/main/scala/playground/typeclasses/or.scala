package playground.typeclasses
import scala.annotation.tailrec

trait Eq[A]:
  extension (a: A) def ===(b: A): Boolean

object Eq:
  final given listEqs[A](using Eq[A]): Eq[List[A]] with
    extension (as: List[A])
      @tailrec def ===(bs: List[A]) = as match
        case Nil      => bs == Nil
        case a :: as1 =>
          bs match
            case Nil      => false
            case b :: bs1 => a === b && as1 === bs1
