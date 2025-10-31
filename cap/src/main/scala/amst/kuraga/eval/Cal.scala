package amst.kuraga.eval

import scala.caps.Mutable
import scala.annotation.tailrec
import scala.caps.SharedCapability
import amst.kuraga.eval.Cal.FlatMap
import scala.compiletime.Erased
import cats.Eval

sealed abstract class Cal:
  self: Any^ =>
  import Cal.*
  type Out
  def flatMap[B](f: Out => Aux[B]^): Aux[B]^{this, f} = FlatMap[B](this, f)

  def run: Eval[Out]

object Cal {
  type Aux[+O] = Cal { type Out <: O }
  trait Ability                                          extends SharedCapability {
    type Res
    type Introduce
    def run: Eval[Res]
  }
  transparent sealed abstract class Of[+A]               extends Cal              {
    override type Out <: A
  }
  final case class Pure(value: Any)                      extends Cal              {
    override type Out = value.type
    def run = Eval.now(value)
  }
  class FlatMap[B](val l: Cal^, val f: l.Out => Aux[B]^) extends Cal              {
    type Out = B
    def run: Eval[Out] = Eval.defer(l.run).flatMap(f(_).run)
  }
  class Below(val ability: Ability^)                     extends Cal              {
    type Out = ability.Res
    def run: Eval[Out] = ability.run
  }

  // case fm: FlatMap[x, A]^ =>
  //   type X = x
  //   fm.l match
  //     case Pure(a)            => run(fm.f(a))
  //     case gm: FlatMap[y, X]^ => run(gm.l.flatMap(x => gm.f(x).flatMap(fm.f)))

}
