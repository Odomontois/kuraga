package playground.typeclasses
import scala.annotation.tailrec

trait Eq[A]:
    def (a: A) === (b: A): Boolean

object Eq:
    final given listEqs[A] (using  Eq[A]) as Eq[List[A]]:
        @tailrec def (as: List[A]) === (bs: List[A]) = as match
            case Nil => bs == Nil
            case a :: as1 => bs match 
                case Nil => false
                case b :: bs1 => a === b && as1 === bs1            

