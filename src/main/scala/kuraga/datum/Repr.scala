package kuraga.datum

import Repr.Holder

object Repr:
  trait Holder:
    type T

trait Repr[-P[-_, +_], +X]:
  def viaHolder(holder: Holder): P[X, holder.T] => holder.T

  def apply[A](p: P[X, A]): A = viaHolder(new { type T = A })(p)
end Repr
