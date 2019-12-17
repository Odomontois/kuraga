package playground
package data.layer
import scala.annotation.tailrec

trait Layer[-P[-_, _]]
    def peel[R](cont: P[Layer[P], R]) : R

trait WithCat[-I, +O]
    def cat(name: String, fur: String, rest: I): O

object WithCat
    def apply[P <: WithCat](name: String, fur: String, rest: Layer[P]): Layer[P] = 
        new {
            def peel[R](cont: P[Layer[P], R]): R = cont.cat(name, fur, rest)
        }        

trait WithDog[-I, +O]
    def dog(name: String, size: Long, rest: I): O

object WithDog
    def apply[P <: WithDog](name: String, size: Long, rest: Layer[P]): Layer[P] = 
        new {
            def peel[R](cont: P[Layer[P], R]): R = cont.dog(name, size, rest)
        }

trait End[+O]
    def end: O

object End extends Layer[[-i, o] =>> End[o]]
    def peel[R](cont: End[R]): R = cont.end