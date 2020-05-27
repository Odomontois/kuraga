package playground.chat.kaivessel
// //port of https://scastie.scala-lang.org/1QH1bFGSRcSkcGavkm0Zbw 

trait View[C[f[_]]]:
  def nullV[F[_]](container: C[F]): Boolean
  final def collapseNullV[F[_]](container: C[F]): Option[C[F]] = if (nullV(container)) None else Some(container)

type FlipAp[G[_]] = [c[f[_]]] =>> c[G]

sealed trait VSum[K <: K3, G[_]]:
  type A[_[_]]
  val key: K[A]
  val value: A[G]
  final def toDPair: DPair[K2, K, FlipAp[G]] = key -> (value : FlipAp[G][A])

object VSum:
  def apply[K <: K3, V[f[_]], G[_]](k: K[V], v: V[G]):  VSum[K, G] = new:
      type A[f[_]] = V[f]
      val key = k
      val value = v

  given pair [K <: K3, V <: K2, G[_]] as Conversion[(K[V], V[G]), VSum[K, G]] = 
    tup => apply[K, V, G](tup._1, tup._2)

object Vessel:
  opaque type Vessel[K <: K3, G[_]] = MonoidalDMap[K2, K, FlipAp[G]]
  // hs:fromListV
  def apply[K <: K3, G[_]](elems: VSum[K, G]*)(using Has[K2, View, K]): Vessel[K, G] = 
    MonoidalDMap((for vsum <- elems if !vsum.key.constraintsFor.nullV(vsum.value) yield vsum.toDPair ): _*)
    
  private def [K <: K3, G[_]] (map: MonoidalDMap[K2, K, FlipAp[G]]) filterNullFlipAps 
    (using Has[K2, View, K]): MonoidalDMap[K2, K, FlipAp[G]] =
    val items = 
      for e <- map.asList.toList
          v <- e.key.constraintsFor.collapseNullV[G](e.value)
      yield DPair[K2, K, FlipAp[G], e.A](e.key,  v)

    MonoidalDMap.apply[K2, K, FlipAp[G]](items: _*)     

  extension on [K <: K3, G[_]](self: Vessel[K, G]):
    def set(vsum: VSum[K, G])(using Eq2[K], Has0[Semigroup, FlipAp[G], K], Has[K2, View, K]): Vessel[K, G] =
      self ++ Vessel(vsum)
    // hs:Semigroup (but right-biased of course)
    def ++(that: Vessel[K, G])(using Eq2[K], Has0[Semigroup, FlipAp[G], K], Has[K2, View, K]): Vessel[K, G] =
      filterNullFlipAps(MonoidalDMap.ops.++(self)(that))
    def toList: List[VSum[K, G]] =
      self.asList.toList.map(e => VSum[K, e.A, G](e.key, e.value))

  given[K <: K3: Eq2](using Has[K2, View, K]) as View[[f[_]] =>> Vessel[K, f]]:
    def nullV[F[_]](container: Vessel[K, F]) = container.toList.isEmpty 

type Vessel[K <: K3, G <: K1] = Vessel.Vessel[K, G]
