package amst.uu
import scala.caps.Capability
import scala.caps.cap
import scala.caps.SharedCapability

class Foo extends SharedCapability
class Bar(u: Any)

def xx(f: Foo -> String): String = f(Foo())
def yy(f: Bar -> String): String = f(Bar(()))

@main def zz() =
  val foo: Foo^{cap}          = Foo()
  val f1: Foo^{cap} -> String = _ => "lol"
  val f2: Bar^{foo} -> String = _ => "kek"
  println(xx(f1))
  println(yy(f2))
