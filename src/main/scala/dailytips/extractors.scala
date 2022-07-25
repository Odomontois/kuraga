package dailytips.unchimney

import compiletime.constValue

object ByKey:
  def unapplySeq[K, V](kv: Map[K, V]): LazyList[Map[K, V]] =
    LazyList.continually(kv)

object --> :
  inline def unapply[K1 <: K & Singleton, K, V](bk: Map[K, V]): Option[(K1, V)] =
    val k = constValue[K1]
    bk.get(k).map(k -> _)

val x = Map("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4)

@main def foo() =
  x match
    case ByKey("x" --> x, _*)            => println(s"x: $x")
    case ByKey("c" --> c, "a" --> a, _*) => println(s"c: $c, a: $a")
