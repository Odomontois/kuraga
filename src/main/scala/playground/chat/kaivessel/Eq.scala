package playground.chat.kaivessel
import Eq.GEQ


trait Is[k <: *, A <: k, B <: k]:
    def substitute[F[_ <: k]](fa: F[A]): F[B]

    final def reverse: Is[k, B, A] = substitute[[a <: k] =>> Is[k, a, A]](???)

object Is:
    private def refl0[k <: AnyKind, A <: k] : Is[k, A, A] = new:
        def substitute[F[_ <: k]](fa: F[A]): F[A] = fa
    private val reflAny : Is[Any, Any, Any] = refl0

    def refl[k <: AnyKind, A <: k]: Is[k, A, A] = reflAny.asInstanceOf[Is[k, A, A]]


// hs:GCompare
trait Eq[k <: AnyKind, K[_ <: k]] :
  extension [A <: k, B <: k](k: K[A]) def isEq  (k2: K[B]): [R] => GEQ[k, K, A, B, R] => R

object Eq:
  trait GEQ[k <: AnyKind, K[_ <: k], A <: k, B <: k, R]:
    def Y(res: K[A], is: Is[k, A, B]): R
    def N: R

type Eq0[K[_]] = Eq[Any, K] 
type Eq1[K[_[_]]] = Eq[K1, K] 
type Eq2[K[_[_[_]]]] = Eq[K2, K] 