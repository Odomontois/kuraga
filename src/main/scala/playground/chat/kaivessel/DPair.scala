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
  given pair [k, K[_ <: k], V[_ <: k], C <: k] as Conversion[(K[C], V[C]), DPair[k, K, V]] = tup => 
    case class dpair(key: K[C], value: V[C]) extends DPair[k, K, V]:
      type A = C
    dpair(tup._1, tup._2)  