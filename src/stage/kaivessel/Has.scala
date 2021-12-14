package playground.chat.kaivessel

trait Has[k <: *, TC[_ <: k], GADT[_ <: k]]:
  def cts4[A <: k](gadt: GADT[A]): TC[A] = gadt.constraintsFor
  extension [A <: k](gadt: GADT[A]) def constraintsFor: TC[A]

type TC1[TC[_], V[_[_[_]]]]                 = [A[_[_]]] =>> TC[V[A]]
type Has0[TC[_], V[_[_[_]]], GADT[_[_[_]]]] = Has[K2, TC1[TC, V], GADT]
