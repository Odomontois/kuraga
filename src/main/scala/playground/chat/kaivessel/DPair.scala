package playground.chat.kaivessel

sealed trait DPair[k <: AnyKind, K[_ <: k], V[_ <: k]]:
  type A <: k
  val key: K[A]
  val value: V[A]   
  final def extract[B <: k](k: K[B])(using Eq[k, K]): Option[V[B]] = key.isEq[A, B](k) match 
    case y: Eq.GEQ.Y[k, K, A, B] => Some(y.is.substitute[V](value))
    case _                => None

// object DPair:
//   given pair [k, K[_ <: k], V[_ <: k], C <: k] as Conversion[(K[C], V[C]), DPair[k, K, V]] = tup => 
//     case class dpair(key: K[C], value: V[C]) extends DPair[k, K, V]:
//       type A = C
//     dpair(tup._1, tup._2)  