package playground.chat.kaivessel

trait Has[k <: *, TC <: ^[k], GADT <: ^[k]]:
  def [A <: k](gadt: GADT[A]) constraintsFor: TC[A]
  def cts4[A <: k](gadt: GADT[A]): TC[A] = gadt.constraintsFor[A]

type TC1[TC[_], V <: K3] = [A[_[_]]] =>> TC[V[A]]
type Has0[TC[_], V <: K3, GADT <: K3] = Has[K2, TC1[TC, V], GADT]