package playground.talks.iomonad

import scala.annotation.tailrec
import IO._
import scala.util.control.NonFatal
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.concurrent.ExecutionContext

enum IO[+A]:
  def flatMap[B](f: A => IO[B]): IO[B]                    =
    Continue(this, _.fold(Throw(_), f))
  def handleWith[A1 >: A](h: Throwable => IO[A1]): IO[A1] =
    Continue(this, _.fold(h, Pure(_)))

  case Pure(a: A)
  case Throw(err: Throwable)
  case Continue[A, +B](a: IO[A], f: Try[A] => IO[B]) extends IO[B]
  case Async(cont: (Try[A] => Unit) => Unit, ec: ExecutionContext)

  @tailrec private def unsafeRun1(callback: Try[A] => Unit): Unit = this match
    case Pure(a)         => callback(Success(a))
    case Throw(err)      => callback(Failure(err))
    case Async(cont, _)  => cont(callback)
    case Continue(fa, f) =>
      fa match
        case Pure(a)         => f(Success(a)).unsafeRun1(callback)
        case Throw(err)      => f(Failure(err)).unsafeRun1(callback)
        case Continue(fx, g) => Continue(fx, u => Continue(g(u), f)).unsafeRun1(callback)
        case Async(cont, ec) => cont(e => ec.execute(() => f(e).unsafeRun(callback)))

  final def unsafeRun(callback: Try[A] => Unit): Unit             =
    try unsafeRun1(callback)
    catch { case NonFatal(err) => callback(Failure(err)) }

// @tailrec final def unsafeRun(): Res[A] = this match
//   case res: Res[A]                     => res
//   case Continue(io, f): Continue[x, A] =>
//     type X = x
//     io match
//       case res: Res[X]     =>
//         val next =
//           try f(res)
//           catch case NonFatal(err) => Throw(err)
//         next.unsafeRun()
//       case Continue(fx, g) => Continue(fx, ta => Continue(g(ta), f)).unsafeRun()
