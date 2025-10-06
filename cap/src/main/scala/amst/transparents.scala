package amst.transparents

import scala.annotation.tailrec

//very poor performance pseudo monad
sealed trait Tail[+A] {
  import Tail.*
  def flatMap[B](f: A => Tail[B]): Tail[B]^{this, f} = this match
    case Now(value)  => f(value).later
    case l: Later[A] => l.step().flatMap(f).later

  def map[B](f: A => B): Tail[B]^{this, f} = this match
    case Now(value)  => Now(f(value)).later
    case l: Later[A] => l.step().map(f).later

  @tailrec final def result: A = this match
    case Now(value)         => value
    case l: Later[A]^{this} => l.step().result

}

object Tail {
  case class Now[+A](value: A) extends Tail[A]
  abstract class Later[+A]     extends Tail[A]:
    self: Any^ =>
    def step(): Tail[A]^{this}

  extension [A](inline x: => Tail[A]^)
    // if you try to remove transparent here, the example wouldn't compile
    transparent inline def later: Later[A] = new Later:
      def step() = x
}

@main def run() =
  def repeat[A](n: Int, start: A)(f: A => A): Tail[A]^{f} =
    if n == 0 then Tail.Now(start)
    else for x <- repeat(n - 1, start)(f).later yield f(x)

  println:
    repeat(100_000, BigInt(1))(_ + 1).result
