package playground.tracks

import scala.language.experimental.modularity


case class Bar(tracked val x: Any)
case class Foo(tracked val x: Int, tracked val bar: Bar)
