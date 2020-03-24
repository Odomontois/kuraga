package playground
package free
import cats._
import cats.effect._
import scala.io.StdIn.readLine

enum Console[+A](action: => A){
  case GetLine              extends Console[String](readLine())
  case PutLine(s: String)   extends Console[Unit](println(s))
  def act() = action
}

object Console{
  val getLine: Free[Console, String]            = Free.suspend(GetLine)
  def putLine(s: String): Free[Console, Unit]   = Free.suspend(PutLine(s))
  given RunOr[Console]{
    def[F[_], A]  (c: Console[A] | F[A]) runOr(using Run[F]): Free[Console || F, A] = c match {
      case c: Console[A] => Free(c.act())
      case other: F[A]   => other.run
    }
  }
}


class Ref[A](private var v: A){
  def get: Free[Void, A] = Free(v)
  def set(a: A): Free[Void, Unit] = Free{v = a}
  def modify(f: A => A): Free[Void, Unit] = Free{v = f(v)}
}


