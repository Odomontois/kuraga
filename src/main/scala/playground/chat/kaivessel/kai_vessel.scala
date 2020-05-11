// package playground.chat.kaivessel
// //port of https://scastie.scala-lang.org/1QH1bFGSRcSkcGavkm0Zbw 

// import Eq.{GEQ}
// import GEQ.{Y, N}
// import Vessel.Vessel
// import MonoidalDMap.MonoidalDMap
// import scala.deriving.Mirror


// // hs:ArgDict, Has
// trait Has[TC <: K3, GADT <: K3]:
//   def [A[_[_]]](gadt: GADT[A]) constraintsFor: TC[A]

// object Has:
//   given [TC <: K3](using tcInt: TC[LI], tcString: TC[LS]) as Has[TC, ExampleGADT]:
//     def [A[_[_]]](gadt: ExampleGADT[A])constraintsFor: TC[A] = gadt match
//         case ExampleGADT.S => tcString
//         case ExampleGADT.I => tcInt

// // hs:Has'
// type Has0[TC[_], V <: K3, GADT <: K3] = Has[[A[_[_]]] =>> TC[V[A]], GADT]



// object MonoidalDMap:
//   opaque type MonoidalDMap[K <: K3, +V <: K3] <: DGet[K, V] with DList[K, V] = DMap[K, V]  

//   def empty[K <: K3, V <: K3]: MonoidalDMap[K, V]                      = DMap.empty[K, V]
//   def apply[K <: K3, V <: K3](elems: DPair[K, V]*): MonoidalDMap[K, V] = DMap(elems: _*)

//   extension ops on [K <: K3, V <: K3](self: MonoidalDMap[K, V]):
//     def ++(that: MonoidalDMap[K, V])(using Eq[K], Has0[Semigroup, V, K]): MonoidalDMap[K, V] = 
//       val luniq = self -- that
//       val runiq = that -- self
//       val intersection: List[DPair[K, V]] = 
//         for 
//           e  <- self.toList
//           v2 <- that.get(e.key)
//           v1 = e.value
//           sg = e.key.constraintsFor
//         yield e.key -> sg.combine(v1, v2)      
      
//       MonoidalDMap(luniq.toList ++ runiq.toList ++ intersection: _*)   
    
//     def set(kv: DPair[K, V])(using Eq[K]): MonoidalDMap[K, V]         = self.set(kv)
//     def --(that: MonoidalDMap[K, V])(using Eq[K]): MonoidalDMap[K, V] = self -- that     

// trait View[c[f[_]]]:
//   def nullV[F[_]](container: c[F]): Boolean
//   final def collapseNullV[F[_]](container: c[F]): Option[c[F]] = if (nullV(container)) None else Some(container)

// object View:
//   given View[LS]:
//     def nullV[F[_]](container: LS[F]) = container.l.isEmpty
  
//   given View[LI]:
//     def nullV[F[_]](container: LI[F]) = container.l.isEmpty  

// type FlipAp[G[_]] = [c[f[_]]] =>> c[G]

// sealed trait VSum[K <: K3, G[_]]:
//   type A[_[_]]
//   val key: K[A]
//   val value: A[G]
//   final def toDPair: DPair[K, FlipAp[G]] = key -> (value : FlipAp[G][A])

// object VSum:
//   given[K <: K3, V[f[_]], G[_]] as Conversion[(K[V], V[G]), VSum[K, G]] = 
//     tup => new: 
//       type A[f[_]] = V[f]
//       val (key, value) = tup  

// object Vessel:
//   opaque type Vessel[K <: K3, G[_]] <: DGet[K, FlipAp[G]] = MonoidalDMap[K, FlipAp[G]]
//   // hs:fromListV
//   def apply[K <: K3, G[_]](elems: VSum[K, G]*)(using Has[View, K]): Vessel[K, G] = 
//     MonoidalDMap((for vsum <- elems if !vsum.key.constraintsFor.nullV(vsum.value) yield vsum.toDPair ): _*)
    
//   private def [K <: K3, G[_]] (map: MonoidalDMap[K, FlipAp[G]]) filterNullFlipAps (using Has[View, K]): MonoidalDMap[K, FlipAp[G]] =
//     MonoidalDMap((
//       for e                 <- map.toList
//           v: FlipAp[G][e.A] <- e.key.constraintsFor.collapseNullV(e.value)
//       yield DPair.pair(e.key -> v)
//     ): _*)     

//   extension on [K <: K3, G[_]](self: Vessel[K, G]):
//     def set(vsum: VSum[K, G])(using Eq[K], Has0[Semigroup, FlipAp[G], K], Has[View, K]): Vessel[K, G] =
//       self ++ Vessel(vsum)
//     // hs:Semigroup (but right-biased of course)
//     def ++(that: Vessel[K, G])(using Eq[K], Has0[Semigroup, FlipAp[G], K], Has[View, K]): Vessel[K, G] =
//       filterNullFlipAps(MonoidalDMap.ops.++(self)(that))
//     def toList: List[VSum[K, G]] =
//       self.toList.map(e => e.key -> e.value)

//   given[K <: K3: Eq](using Has[View, K]) as View[[f[_]] =>> Vessel[K, f]]:
//     def nullV[F[_]](container: Vessel[K, F]) = container.toList.isEmpty 


// @main def checkVessel() = 
//   import ExampleGADT.{S, I}
//   val monoidalDMap =
//     MonoidalDMap(S -> ExampleValue[LS](LS(List(Option("str"))))) ++
//       MonoidalDMap(I -> ExampleValue[LI](LI(List(Option(1)))))

//   val vessel =
//     Vessel(S -> LS[Option](List(Option("str")))) ++
//     Vessel(I -> LI[Option](List(Option(1))))

//   println(monoidalDMap: MonoidalDMap[ExampleGADT, ExampleValue])
//   println(vessel: Vessel[ExampleGADT, Option])