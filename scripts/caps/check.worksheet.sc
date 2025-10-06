//> using scala 3.7.3
//> using options "-language:experimental.captureChecking"

import scala.caps.{Capability, cap}

class Foo extends Capability
class Bar(u: Any)

def xx(f: Foo -> Unit): Unit = f(Foo())

def zz() = 
  val foo: Foo^{cap} = Foo()
  val bar: Bar^{foo} = Bar(foo)
  println("lol")


1 + 2
