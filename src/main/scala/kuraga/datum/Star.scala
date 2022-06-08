package kuraga.datum

import cats.Foldable
import kuraga.datum.Handle.Identity

import scala.annotation.targetName

@targetName("Und")
type &::[A[-_, +_], B[-_, +_]] = [x, y] =>> A[x, y] & B[x, y]

type AnyP[-x, +y] = Any

trait Proeff[-X, +Y]:
  type That[-x, +y] <: Proeff[x, y]
  type Self >: this.type <: That[X, Y]
  def map[Z](f: Y => Z): That[X, Z]
end Proeff

object Proeff:
  type Handler[-O[-_, +_], +R] = Proeff[Star[Nothing, Any], Star[O, R]]
end Proeff

class Handler[+I[-_, +_], -O[-_, +_], +R](val values: Map[String, Proeff.Handler[O, R]]) extends Selectable:
  def handle[U[-_, +_], S]: I[Star[U, S], Star[U &:: O, S | R]]                             = this.asInstanceOf[I[Star[U, S], Star[U &:: O, S | R]]]
  def selectDynamic(name: String): Any                                                      = values(name)
  def ++[V[-_, +_], L[-_, +_], S](that: Handler[V, L, S]): Handler[I &:: V, O &:: L, R | S] =
    Handler(values ++ that.values)
  def andThen[T[-_, +_], S](that: Handler[O, T, S]): Handler[I, T, R | S]                   =
    def mapstar(star: Star[O, R]) = Star.Elim[O, T, R | S](() => star, that)
    Handler(values.view.mapValues(_.map(mapstar)).toMap)
end Handler

object Handle:
  object Identity extends Handler[AnyP, AnyP, Nothing](Map.empty)
end Handle

enum Star[-P[-_, +_], +R]:
  case Intro[Z, -P[-_, +_], +R](f: [A] => P[Star[P, R], A] => A)                               extends Star[P, R]
  case Elim[Q[-_, +_], -P[-_, +_], +R](star: () => Star[P &:: Q, R], handler: Handler[Q, P, R]) extends Star[P, R]
  case Result(r: R)
end Star

object Star:
  extension [R](star: Star[AnyP, R]) def run: R = foldRun(star)(Handle.Identity)
  

  def foldRun[P[-_, +_], S, R](star: Star[P, S])(handler: Handler[P, AnyP, R]): S | R =
    star match
      case Star.Intro(f)              => foldRun(f(handler.handle))(handler)
      case Star.Elim(starIn, handleIn) => foldRun(starIn())(handler ++ handleIn.andThen(handler))
      case Star.Result(x)             => x
end Star

import cats.syntax.all._

def test() =
  val ttt: PartialFunction[Int, String] = { case 1 => "1" }
  val zzz: PartialFunction[Int, String] = { case 2 => "2" }
  List(ttt, zzz).reduce(_ orElse _)
end test
