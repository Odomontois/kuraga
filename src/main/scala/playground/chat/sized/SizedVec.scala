package playground.chat.sized

import scala.compiletime.ops.int._
import scala.compiletime._
import scala.compiletime.testing.typeCheckErrors
import scala.compiletime.ops.boolean.!

object SizedVect extends SizedVectOps:
  opaque type Vec[N <: Int, +A] = Vector[A]

  def empty: Vec[0, Nothing] = Vector.empty

  extension [N <: Int, A](v: Vec[N, A])
    def prepend[A1 >: A](a: A1): Vec[N + 1, A1]                                        = a +: v
    infix def :+[A1 >: A](a: A1): Vec[N + 1, A1]                                       = v :+ a
    infix def ++[M <: Int, A1 >: A](v2: Vec[M, A1]): Vec[N + M, A1]                    = v ++ v2
    def apply[I <: Int with Singleton](i: I)(using I < N =:= true, I >= 0 =:= true): A = v(i)
    def toVector: Vector[A]                                                            = v

trait SizedVectOps:
  type Applied[T <: Tuple] = T match {
    case EmptyTuple => SizedVect[0, Nothing]
    case a *: ts    =>
      Applied[ts] match {
        case SizedVect[n, b] => SizedVect[n + 1, a | b]
      }
  }

// transparent inline def apply[T <: Tuple](x : T) : Applied[T] =
//     inline x match
//         case _ : EmptyTuple => SizedVect.empty
//         case xx : (t *: ts) => apply(xx.tail).prepend(xx.head)

type SizedVect[N <: Int, +A] = SizedVect.Vec[N, A]

@main def foo =
  val x = SizedVect.empty :+ 1 :+ 2 :+ 3
  val y = SizedVect.empty :+ 4 :+ 5 :+ 6

  val z = x ++ y

  z(1)
  z(5)

  typeCheckErrors("""
        z(6)
        z(-1)
    """).foreach(println)

  println(z)
