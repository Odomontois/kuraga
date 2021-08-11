package playground.chat.kaivessel

import Eq.{GEQ}

trait DGet[k <: *, K[_ <: k], +V[_ <: k]]:
  def get[A <: k](k: K[A])(using Eq[k, K]): Option[V[A]]

trait DList[k <: *, K[_ <: k], +V[_ <: k]]:
  def toList: List[DPair[k, K, V]]

sealed trait DMap[k <: *, K[_ <: k], +V[_ <: k]] extends DGet[k, K, V] with DList[k, K, V]:
  def set[V1[a <: k] >: V[a]](kv: DPair[k, K, V1])(using Eq[k, K]): DMap[k, K, V1]
  // right-biased of course
  def ++[V1[a <: k] >: V[a]](that: DMap[k, K, V1])(using Eq[k, K]): DMap[k, K, V1]
  def --[V1[a <: k] >: V[a]](that: DMap[k, K, V1])(using Eq[k, K]): DMap[k, K, V]

object DMap:
  def empty[k <: *, K[_ <: k], V[_ <: k]]: DMap[k, K, V] = apply()
  def apply[k <: *, K[_ <: k], V[_ <: k]](elems: DPair[k, K, V]*): DMap[k, K, V] =
    case class dmap[V1[a <: k] >: V[a]](elems: Seq[DPair[k, K, V1]]) extends DMap[k, K, V1]:
      def get[A <: k](k: K[A])(using Eq[k, K])                          = elems.collectFirst(Function.unlift((_: DPair[k, K, V1]).extract(k)))
      def ++[V2[a <: k] >: V1[a]](that: DMap[k, K, V2])(using Eq[k, K]) = dmap((this -- that).toList ++ that.toList)
      def --[V2[a <: k] >: V1[a]](that: DMap[k, K, V2])(using Eq[k, K]) = dmap(
        elems.filter(e => that.get(e.key).isEmpty)
      )
      def set[V2[a <: k] >: V1[a]](kv: DPair[k, K, V2])(using Eq[k, K]) = this ++ DMap(kv)
      def toList: List[DPair[k, K, V1]]                                 = elems.toList
    dmap(elems)
