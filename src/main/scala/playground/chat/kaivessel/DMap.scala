// package playground.chat.kaivessel
// //port of https://scastie.scala-lang.org/1QH1bFGSRcSkcGavkm0Zbw 

// import Eq.{GEQ}
// import GEQ.{Y, N}
// import Vessel.Vessel
// import MonoidalDMap.MonoidalDMap
// import scala.deriving.Mirror


// trait DGet[k <: *, K <: K3, +V <: K3]:
//   def get[A[_[_]]](k: K[A])(using Eq[K]): Option[V[A]]

// trait DList[K <: K3, +V <: K3]:
//   def toList: List[DPair[K, V]]

// sealed trait DMap[K <: K3, +V <: K3] extends DGet[K, V] with DList[K, V]:  
//   def set[V1 >: V <: K3](kv: DPair[K, V1])(using Eq[K]): DMap[K, V1]
//   // right-biased of course
//   def ++[V1 >: V <: K3](that: DMap[K, V1])(using Eq[K]): DMap[K, V1]
//   def --[V1 >: V <: K3](that: DMap[K, V1])(using Eq[K]): DMap[K, V]  

// object DMap:
//   def empty[K <: K3, V <: K3]: DMap[K, V] = apply()
//   def apply[K <: K3, V <: K3](elems: DPair[K, V]*): DMap[K, V] = 
//     case class dmap[V1 >: V <: K3](elems: Seq[DPair[K, V1]]) extends DMap[K, V1] :
//       def get[A[_[_]]](k: K[A])(using Eq[K]) = elems.collectFirst(Function.unlift((_: DPair[K, V1]).extract(k)))      
//       def ++[V2 >: V1 <: K3](that: DMap[K, V2])(using Eq[K])  = dmap((this -- that).toList ++ that.toList)      
//       def --[V2 >: V1 <: K3](that: DMap[K, V2])(using Eq[K])  = dmap(elems.filter(e => that.get(e.key).isEmpty))      
//       def set[V2>: V1 <: K3](kv: DPair[K, V2])(using Eq[K])  = this ++ DMap(kv)
//       def toList: List[DPair[K, V1]]          = elems.toList    
//     dmap(elems)