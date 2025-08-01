package playground.tracks

import scala.language.experimental.modularity

case class Bar(tracked val x: Any)
case class Foo(tracked val x: Int, tracked val bar: Bar)
// i
@main def lol() =
  val foo = Foo(3, Bar(Bar(1)))
  val foo1 : Foo(3, Bar(Bar(1))) = foo
  println(s"lolliess $foo")
