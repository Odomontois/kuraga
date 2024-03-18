package kuraga.typenum
import kuraga.typenum.Tagged
trait Foo[+A]

given Foo[String] with {}

given Foo[Int] with {}

// @main def run = 
//     println(Tagged.findAll[Foo])


