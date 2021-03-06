package kuraga
import scala.annotation.tailrec
import Eval.{defer, delay}

type ~>[F[_], G[_]] = [A] => F[A] => G[A]

opaque type Endo[A] = Eval[A] => Eval[A]

object Endo:
  def apply[A](f: Eval[A] => Eval[A]): Endo[A] = f

  extension [A](endo: Endo[A]) def apply(ea: Eval[A]): Eval[A] = endo(ea)

  given [A]: Monoid[Endo[A]] with
    def default                                                = x => x
    extension (x: Endo[A]) def combine(y: Endo[A])             = e => x(y(e)).defer
    extension (lx: Eval[Endo[A]])
      override def combineLz(ly: Eval[Endo[A]]): Eval[Endo[A]] =
        Eval.now(la => lx.flatMap(_(ly.flatMap(_(la)))))

class DelayedFunc[A, B](f: => A => B)             extends (A => B):
  def apply(a: A): B = f(a)

class FuncByNeed[A, B](f: A => B)                 extends (A => B):
  lazy val fmemo     = f
  def apply(a: A): B = fmemo(a)

case class Compose[A, B, C](f: A => B, g: B => C) extends (A => C):
  @tailrec final def apply(a: A): C          = f match
    case Compose(u, v) => Compose(u, Compose(v, g))(a)
    case _             => standardApply(a)
  @inline private def standardApply(a: A): C = g(f(a))

opaque type EndoE[A] = A => A
object EndoE:
  def apply[A](f: A => A): EndoE[A] = f

  extension [A](endo: EndoE[A]) def run: A => A = endo

  given [A]: Monoid[EndoE[A]] with
    def default                                      = x => x
    extension (x: EndoE[A]) def combine(y: EndoE[A]) = Compose(x, y)
    extension (lx: Eval[EndoE[A]])
      override def combineLz(ly: Eval[EndoE[A]])     =
        Compose(lx.value, DelayedFunc(ly.value)).delay
