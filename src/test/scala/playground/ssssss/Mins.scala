package playground.ssssss

type &::[A[-_, +_], B[-_, +_]] = [x, y] =>> A[x, y] & B[x, y]

class Handler[+I[-_, +_], -O[-_, +_], +R](val values: Map[String, Proeff.Handle[O, R]]) extends Selectable:
  def handle[U[-_, +_], S]: I[Star[U, S], Star[U &:: O, S | R]]                             = this.asInstanceOf[I[Star[U, S], Star[U &:: O, S | R]]]
  def selectDynamic(name: String): Any                                                      = values(name)
  def ++[V[-_, +_], L[-_, +_], S](that: Handler[V, L, S]): Handler[I &:: V, O &:: L, R | S] =
    Handler(values ++ that.values)
  def andThen[T[-_, +_], S](that: Handler[O, T, S]): Handler[I, T, R | S]                   =
    def mapstar(star: Star[O, R]) = Star.Elim[O, T, R | S](star, that)
    Handler(values.view.mapValues(_.map(mapstar)).toMap)

end Handler

object Handler:
  object Identity extends Handler[AnyP, AnyP, Nothing](Map.empty)

  opaque type DefaultName[P[-_, +_]] = String

  def DefaultName[P[-_, +_]](s: String): DefaultName[P] = s

  def single[I[-x, +y], O[-_, +_], R](
      name: String,
      handler: Proeff.Handle[O, R]
  ): Handler[I, O, R] = Handler[I, O, R](Map(name -> handler))

  def singleVia[P[-x, +y] <: Proeff[x, y]](using name: DefaultName[P]) = new SingleVia[P](name)

  class SingleVia[P[-x, +y] <: Proeff[x, y]](val name: DefaultName[P]) extends AnyVal:
    def apply[I[-x, +y], O[-_, +_], R](handler: P[Star[Nothing, Any], Star[O, R]]): Handler[I, O, R] =
      single(name, handler)

  type Of = Handler[AnyP, Nothing, Any]
end Handler

trait Proeff[-X, +Y]:
  def map[Z](f: Y => Z): Proeff[X, Z]
  def me: Proeff[X, Y]
  def name(): String
end Proeff

object Proeff:
  type Handle[-O[-_, +_], +R] = Proeff[Star[Nothing, Any], Star[O, R]]
end Proeff

type AnyP[-x, +y] = Any

enum Star[-P[-_, +_], +R]:
  case Intro[-P[-_, +_], +R](f: Repr[P, Star[P, R]])                                      extends Star[P, R]
  case Elim[Q[-_, +_], -P[-_, +_], +R](star: Star[P &:: Q, R], handler: Handler[Q, P, R]) extends Star[P, R]
  case Done(r: R)

  def elimRun[A](handler: Handler[P, AnyP, A]): R | A = Star.foldRun(this)(handler)
end Star

import Repr.Holder
import playground.ssssss.Handler.DefaultName
import playground.ssssss.Result.Result

object Repr:
  trait Holder:
    type T

trait Repr[-P[-_, +_], +X]:
  def viaHolder(holder: Holder): P[X, holder.T] => holder.T

  def apply[A](p: P[X, A]): A = viaHolder(new { type T = A })(p)
end Repr

object Star:
  extension [R](star: Star[AnyP, R]) def run: R = foldRun(star)(Handler.Identity)

  def foldRun[P[-_, +_], S, R](star: Star[P, S])(handler: Handler[P, AnyP, R]): S | R =
    star match
      case Intro(f)               => foldRun(f(handler.handle))(handler)
      case Elim(starIn, handleIn) => foldRun(starIn)(handler ++ handleIn.andThen(handler))
      case Done(x)                => x
end Star

type GetResult[Q] = Q match
  case Result.Res[_, _, a] => a

trait ResultEff[-A, -X, +Y] extends Proeff[Any, Y]:
  self =>
  def result(a: A): Y


  def me: ResultEff[A, X, Y] = this

  override def map[Z](f: Y => Z): ResultEff[A, X, Z] = new:
    def result(a: A)                 = f(self.result(a))
    override def map[Z1](g: Z => Z1) = self.map(f andThen g)

  override def name() = "Result"
end ResultEff

object ResultEff:
  extension [P[-_, +_], S, A](star: Star[P &:: Result[A], S])
    def flatMap[Q[-_, +_], T](f: A => Star[Q, T]): Star[P &:: Q, S | T] =
      Star.Elim[Result[A], P &:: Q, S | T](star, Result.handler(f))
end ResultEff

object Result:
  type Eff[-A]         = [x, y] =>> ResultEff[A, x, y]
  type Res[-X, +Y, -A] = Handler.Of { val result: ResultEff[A, X, Y] }
  type Result[-A]      = [X, Y] =>> Res[X, Y, A]

  given [A]: DefaultName[Eff[A]] = DefaultName("result")

  def handler[A, P[-_, +_], R](f: A => Star[P, R]): Handler[Result[A], P, R] =
    Handler.singleVia[Eff[A]](a => f(a))

  def pure[U](a: U): Star[Result[U], Nothing] = Star.Intro(_ => r => r.result.result(a))
end Result

@main def foo() =
  println(Result.handler(x => Star.Done(x)).values.values.map(_.me.name()).mkString(";"))
