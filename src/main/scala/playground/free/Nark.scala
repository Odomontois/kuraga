package playground.free

import scala.annotation.tailrec

infix type |:|[F[_], G[_]] = [a] =>> F[a] | G[a]

sealed trait Nark[+F[+_]]:
  import Nark._

  def reset[G[+_]](up: Unpack[F, G]): Nark[G] = Reset(this, up)

  def resetIter[G[+_]](up: Embed[F, G] => Nark[G]): Nark[G] = Reset(this, _.resetIter(up))

  @tailrec final def exec: Yield[F] = this match
    case d: Delay[F]                      => d.step().exec
    case y: Yield[F]                      => y
    case Reset(pack, unpack): Reset[g, F] =>
      type G[+x] = g[x]
      pack match
        case d: Delay[G]           => Reset(d.step(), unpack).exec
        case y: Yield[G]           => unpack(y).exec
        case Reset(pack2, unpack2) => Reset(pack2, unpack2.compose(unpack)).exec

  def partialMatch[L[+x] <: Matchable, R[+x]](using F <::< (L |:| R))(f: Embed[L, R] => Nark[R]): Nark[R] =
    import <::<.given
    resetIter(e => e.split[L, R, Nark[R]](f, _.asYield))

  def flatMap[A, F1[+x]](using F <::< (PureT[A] |:| F1))(f: A => Nark[F1]): Nark[F1] =
    partialMatch(e => f(e.pivot.value))

  def handleError[E, F1[+x]](using F <::< (ErrT[E] |:| F1))(f: E => Nark[F1]): Nark[F1] =
    partialMatch(e => f(e.pivot.err))

  def provide[R, F1[+x]](using F <::< (Read[R, *] |:| F1))(r: R): Nark[F1] = 
    partialMatch(e => e.cont(e.pivot.run(r)))

object Nark:

  trait Delay[+F[+_]] extends Nark[F]:
    def step(): Nark[F]

  def delay[F[+_]](d: Delay[F]): Delay[F] = d

  trait Embed[+F[+_], +G[+_]]:
    self =>
    type P
    def pivot: F[P]
    def cont(p: P): Nark[G]

    def asYield[H[+x] >: F[x] | G[x]]: Yield[H] = new:
      export self._

    final def split[L[+x] <: Matchable, R[+x], V](
        l: Embed[L, G] => V,
        r: Embed[R, G] => V
    )(using F[P] <:< (L[P] | R[P])): V =
      pivot match
        case lp: L[P] @unchecked => l(this.asInstanceOf[Embed[L, G]])
        case rp                  => r(this.asInstanceOf[Embed[R, G]])

  trait Yield[+F[+_]]          extends Nark[F] with Embed[F, F]:
    self =>
    override def resetIter[G[+_]](up: Embed[F, G] => Nark[G]): Nark[G] = up(new:
      type P = self.P
      export self.pivot
      def cont(p: P): Nark[G] = delay(() => self.cont(p).resetIter(up))
    )

    override def asYield[H[+x] >: F[x]]: Yield[H] = this

  trait Unpack[-F[+_], +G[+_]] extends (Yield[F] => Nark[G]):
    self =>
    def compose[H[+_]](u: Unpack[G, H]): Unpack[F, H] =
      ff => Reset(self(ff), gg => delay(() => u(gg)))

  case class Reset[F[+_], +G[+_]](pack: Nark[F], unpack: Unpack[F, G]) extends Nark[G]

  final case class Pure[+V](value: V)
  type PureT[+V] = [a] =>> Pure[V]

  case class Err[+E](err: E)
  type ErrT[+E] = [a] =>> Err[E]

  trait Read[-R, +A]:
    def run(r: R): A

abstract sealed class <::<[-F[_], +G[_]]:
  def liftK[H[+f[_]]](hf: H[F]): H[G]

  final def apply[A](fa: F[A]): G[A]          = liftK[[f[_]] =>> f[A]](fa)
  final def coliftK[H[-f[_]]](hg: H[G]): H[F] =
    liftK[[z[_]] =>> H[z] <:< H[F]](<:<.refl)(hg)

object <::< :
  given ident[F[_]]: (F <::< F) with
    def liftK[H[+f[_]]](hf: H[F]): H[F] = hf

  given [F[_], G[_], A](using sub: F <::< G): (F[A] <:< G[A]) =
    sub.liftK[[z[_]] =>> F[A] <:< z[A]](<:<.refl)
