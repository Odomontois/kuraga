package playground.chat.kaivessel
// import language.experimental.namedTypeArguments

sealed trait DPair[k <: *, K[_ <: k], +V[_ <: k]]:
    type A <: k
    val key: K[A]
    val value: V[A]   
    final def extract[B <: k](k: K[B])(using Eq[k, K]): Option[V[B]] = 
        key.isEq(k) `apply` new:
          def Y(k: K[A], is: Is[k, A, B]) = Some(is.substitute[V](value))
          def N = None
    

object DPair:
    def apply[k <: *, K[_ <: k], V[_ <: k], C <: k](key: K[C], value: V[C]): DPair[k, K, V] = 
      case class dpair(key: K[C], value: V[C]) extends DPair[k, K, V]{type A = C}
      dpair(key, value)

    def pair [k <: *, K[_ <: k], V[_ <: k], C <: k]: Conversion[(K[C], V[C]), DPair[k, K, V]] = 
        tup => apply[k, K, V, C](tup._1, tup._2)

    given pair0[K[_], V[_], C] :  Conversion[(K[C], V[C]), DPair[K0, K, V]] = pair
    given pair1[K[_[_]], V[_[_]], C[_]] :  Conversion[(K[C], V[C]), DPair[K1, K, V]] = pair
    given pair2[K[_[_[_]]], V[_[_[_]]], C[_[_]]] :  Conversion[(K[C], V[C]), DPair[K2, K, V]] = pair