package playground.free

import scala.annotation.tailrec

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
  extension [F[+_], A](nr: Nark[[a] =>> F[a] | Pure[A]])
    def flatMap[G[+_]](f: A => Nark[G]): Nark[[a] =>> F[a] | G[a]] =
      nr.reset(e =>
        e.pivot match
          case p: Pure[A] => f(p.value)
          case fp: F[e.P] =>
            new Yield[[a] =>> F[a] | G[a]]:
              type P = e.P
              def pivot      = fp
              def cont(p: P) = e.cont(p).flatMap(f)
      )

  case class Err[+E](e: E)
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
