package amst.dependent

import scala.caps.Capability
import scala.caps.SharedCapability
import scala.caps.ExclusiveCapability

class Foo
class Bar {
  def aaa(f: (foo: Foo) => Foo^) = ()
}

class Kek(foo: Foo^, fooer: (foo: Foo) => Foo^)
object check extends ExclusiveCapability:
  val x: Foo => Foo^ = x => x

  Bar().aaa(a => x(a))
  Kek(Foo(), a => x(a))
