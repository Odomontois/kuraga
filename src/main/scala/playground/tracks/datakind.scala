package playground.tracks

import scala.language.experimental.modularity


@main def lol() =
  val foo = Foo(3, Bar(Bar(1)))
  val foo1 : Foo(3, Bar(Bar(1))) = foo
  println(s"lolliess $foo")
