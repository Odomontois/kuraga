package playground.typeclasses.derivation
import cats.Order
import cats.kernel.Semigroup

object MaxInt extends Newt(_[Int][Numeric][Order]):
    given Semigroup[T] with
        def combine(x: T, y: T) = apply(x.value max y.value)

type MaxInt = MaxInt.T

@main def go() = 
    import cats.implicits.given
    import Numeric.Implicits.given
    val x = MaxInt(1)
    val y = MaxInt(2)

    println(x + y)
    println(x < y)
    println(x |+| y)

    
