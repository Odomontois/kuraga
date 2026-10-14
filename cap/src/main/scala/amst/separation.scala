package amst.uu
import scala.caps.Capability
import scala.caps.SharedCapability
import scala.caps.ExclusiveCapability
import language.experimental.separationChecking
import scala.caps.Mutable
import scala.annotation.unroll


open class Foo extends Mutable:
    def lol(x: Int): Int = x

open class Bar(foo: Foo^) extends Mutable:
    def kek(x: Int): Int = x



def jj: (Int => Int, Int => Int) = 
    val foo: Foo^  = Foo()
    val bar: Bar^ = Bar(foo)

    (x => bar.kek(x), /*foo.lol */ null)
