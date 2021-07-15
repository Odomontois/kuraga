package playground.data

trait Err:
  type Result

object Err:
  private object err extends Err
  type Aux[+E <: Err, A] = E { type Result = A }
  def success[A]: Aux[Err, A] = err.asInstanceOf[Aux[Err, A]]

trait Opt extends Err:
  def none: Result

trait Raisen[-X <: Err]:
  def handle(x: X): x.Result

trait Ctx:
  type Eff[+_]

object Ctx:
  type Aux[+C <: Ctx, F[+x]] = C { type Eff[+a] = F[a] }

  trait Of[F[+_]] extends Ctx:
    type Eff[+a] = F[a]

type CtxLoc[C, F[+_, +_]] = C { type Eff[e, a] = LocalT[F, C, e, a] }

trait LocalT[+F[+_, +_], C, +E, +A] {
  def run[F1[+e, +a] >: F[e, a]](context: CtxLoc[C, F1]): F1[E, A]
}

trait Proc[F[+_], -C <: Ctx, -E <: Err, +A]:
  type P[+x] <: Proc[F, C, E, x]
  def run[B >: A](ctx: Ctx.Aux[C, P], handle: Err.Aux[Err, F[B]]): F[B]

trait Foo[+F[+_]]:
  def foo[F1[+x] >: F[x]]: Unit
