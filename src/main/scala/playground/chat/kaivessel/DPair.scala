package playground.chat.kaivessel

sealed trait DPair[k <: *, K[_ <: k], +V[_ <: k]]:
    type A <: k
    val key: K[A]
    val value: V[A]   
    final def extract[B <: k](k: K[B])(using Eq[k, K]): Option[V[B]] = 
        key.isEq[A, B](k) apply new:
          def Y(k: K[A], is: Is[k, A, B]) = Some(is.substitute[V](value))
          def N = None
    

object DPair:
    def apply[k <: *, K[_ <: k], V[_ <: k], C <: k](key: K[C], value: V[C]): DPair[k, K, V] = 
      case class dpair(key: K[C], value: V[C]) extends DPair[k, K, V]{type A = C}
      dpair(key, value)

    def pair [k <: *, K[_ <: k], V[_ <: k], C <: k]: Conversion[(K[C], V[C]), DPair[k, K, V]] = 
        tup => apply[k, K, V, C](tup._1, tup._2)

    given pair0[K <: K1, V <: K1, C] as Conversion[(K[C], V[C]), DPair[K0, K, V]] = pair
    given pair1[K <: K2, V <: K2, C <: K1] as Conversion[(K[C], V[C]), DPair[K1, K, V]] = pair[k = K1]
    given pair2[K <: K3, V <: K3, C <: K2] as Conversion[(K[C], V[C]), DPair[K2, K, V]] = pair[k = K2]