package optics
import cats.Functor

trait Ctx :
  type P[_, _]
  type F[_]
  type C[+_]



object Ctx{
  class CtxImpl[+UP[_, _], +UF[_], +UC[+_]] extends Ctx{
    type P[x, y] <: UP[x, y]
    type F[x] <: UF[x]
    type C[+x] <: UC[x]
  }
  object CtxObj extends CtxImpl[Nothing, Nothing, Nothing]

  def ctx[UP[_, _], UF[_], UC[+_]]: Ctx{ type P[x, y] <: UP[x, y] ; type F[x] <: UF[x]; type C[+x] <: UC[x] } = CtxObj

  type PLens = Ctx {
    type P[a, b] <: a => b
    type C[+x]   >: Functor[F] ?=> x
  }
}



object types{
  type Optics[S, T, A, B] = (c: Ctx) => c.C[c.P[A, c.F[B]] => c.P[S, c.F[T]]]
  type PLens[S, T, A, B] = (c: Ctx.PLens) => c.C[c.P[A, c.F[B]] => c.P[S, c.F[T]]]

  summon[Ctx.PLens <:< Ctx]
  // def xx[A] = the [((c: Ctx) => A) <:< ((c: Ctx.PLens) => A)]

  // def proof[S, T, A, B] = summon[Optics[S, T, A, B] <:< PLens[S, T, A, B]]
}

