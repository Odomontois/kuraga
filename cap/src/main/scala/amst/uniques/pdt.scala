package amst
package pdt
import scala.annotation.capability
import scala.caps.cap

trait Pair {
    type First
    type Second
    val first: First
    val second: Second

    override def toString = s"($first, $second)"
}

def pair[A, B](a: A, b: B): Pair { type First = A; type Second = B } = new Pair {
    type First  = A
    type Second = B
    val first  = a
    val second = b
}

def rotateLeft(p: Pair { type First <: Pair }): Pair {
    type First  = p.first.First
    type Second = Pair {
        type First  = p.first.Second
        type Second = p.Second
    }
} = pair(p.first.first, pair(p.first.second, p.second))

@capability trait A:
  def actA(): String

@capability trait B:
  def apply(): String


def b(x: A^): B^{} = 
    () => s"performed: ${x.actA()}"


def a: LazyList[Int] = ???
@main def doSomething() = 
   println(b(new A{def actA() = "effect"})())

