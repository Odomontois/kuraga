package playground.typeclasses.derivation

trait Newt[Repr >: LB <: UB, UB, LB, TCS](go: Derives.type => Derives[Repr, UB, LB, TCS]):
    opaque type T >: LB <: UB = Repr

    def apply(t: Repr): T = t

    extension (t: T) def value: Repr = t

    given GND[TC[_]](using Newt.Box[TC] <:< TCS)(using inst: TC[Repr]): TC[T] = inst


object Newt:
    type Box[TC[_]]

trait Derives[Repr >: LB <: UB, UB, LB, TCS]:
    def apply[TC[_]]: Derives[Repr, UB, LB, TCS | Newt.Box[TC]] = new {}

object Derives:
    def apply[Repr]: Derives[Repr, Any, Nothing, Nothing] = new {}

    def Bound[Repr >: LB <: UB, UB, LB]: Derives[Repr, UB, LB, Nothing] = new {}

