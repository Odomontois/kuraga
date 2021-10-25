package playground.talks.iomonad

import scala.annotation.tailrec
import SyncIO._
import scala.util.control.NonFatal
import scala.util.Try
import scala.util.Success
import scala.util.Failure

enum SyncIO[+A]:
  def flatMap[B](f: A => SyncIO[B]): SyncIO[B]                    =
    Continue(this, _.fold(Throw(_), f))
  def handleWith[A1 >: A](h: Throwable => SyncIO[A1]): SyncIO[A1] =
    Continue(this, _.fold(h, Pure(_)))

  case Pure(a: A)
  case Throw(err: Throwable)
  case Continue[A, +B](a: SyncIO[A], f: Try[A] => SyncIO[B]) extends SyncIO[B]

  @tailrec private def unsafeRun1(): Try[A] = this match
    case Pure(a)         => Success(a)
    case Throw(err)      => Failure(err)
    case Continue(fa, f) =>
      fa match
        case Pure(a)         => f(Success(a)).unsafeRun1()
        case Throw(err)      => f(Failure(err)).unsafeRun1()
        case Continue(fx, g) => Continue(fx, ta => Continue(g(ta), f)).unsafeRun1()

  final def unsafeRun(): Try[A]             =
    try unsafeRun1()
    catch { case NonFatal(err) => Failure(err) }

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
