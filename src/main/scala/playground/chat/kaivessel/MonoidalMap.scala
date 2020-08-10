package playground.chat.kaivessel

object MonoidalDMap:
  opaque type MonoidalDMap[k <: *, K[_ <: k], +V[_ <: k]] <: DGet[k, K, V] with DList[k, K, V] = DMap[k, K, V]  

  def empty[k <: *, K[_ <: k], V[_ <: k]]: MonoidalDMap[k, K, V]                         = DMap.empty[k, K, V]
  def apply[k <: *, K[_ <: k], V[_ <: k]](elems: DPair[k, K, V]*): MonoidalDMap[k, K, V] = DMap(elems: _*)

  extension on [k <: *, K[_ <: k], V[_ <: k]](self: MonoidalDMap[k, K, V]):   
    def asList: DList[k, K, V] = self  
    def set(kv: DPair[k, K, V])(using Eq[k, K]): MonoidalDMap[k, K, V]         = self.set(kv)
    def --(that: MonoidalDMap[k, K, V])(using Eq[k, K]): MonoidalDMap[k, K, V] = self -- that  
    

  def concat[K[_[_[_]]], V[_[_[_]]]](
    self: MonoidalDMap[K2, K, V], that: MonoidalDMap[K2, K, V]
    )(using Eq[K2, K], Has0[Semigroup, V, K]): MonoidalDMap[K2, K, V] = 
      val luniq = self -- that
      val runiq = that -- self
      val intersection: List[DPair[K2, K, V]] = 
      for 
        e  <- self.toList
        v2 <- that.get(e.key)
        v1 = e.value
        sg = e.key.constraintsFor
      yield e.key -> sg.combine(v1, v2)
      
      MonoidalDMap(luniq.toList ++ runiq.toList ++ intersection: _*) 
      

  extension ops on[K[_[_[_]]], V[_[_[_]]]](self: MonoidalDMap[K2, K, V]):
    def ++(that: MonoidalDMap[K2, K, V])(using Eq[K2, K], Has0[Semigroup, V, K]): MonoidalDMap[K2, K, V] = concat(that, self)

type MonoidalDMap[k <: *, K[_ <: k], +V[_ <: k]] = MonoidalDMap.MonoidalDMap[k, K, V]