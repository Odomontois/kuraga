package kuraga

import scala.deriving.Mirror

trait DummyInfo[T]:
    def name: String
    def tag: String
    extension (x: T) def info = s"$name<$tag>"  


class DummyDerivation(dummyTag: String):
    trait TC[T] extends DummyInfo[T]
    object TC:
        inline def derived[T](using m: Mirror.Of[T]): TC[T] = new{
            def name = valueOf[m.MirroredLabel]
            def tag = dummyTag
        }


object UnoDerivation extends DummyDerivation("raz")
object DuoDerivation extends DummyDerivation("dva")


case class Fee(name: String) derives UnoDerivation.TC
case object Bar derives DuoDerivation.TC

@main def fooMain() = 
    println(Fee("xx").info)
    println(Bar.info)
