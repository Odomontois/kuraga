package kuraga.indexed

trait IndexedSet[K]{
  type ISet

  def (set: ISet) setMember(k: K): Boolean
  def (set: ISet) insert (k: K): ISet
  def (set: ISet) remove (k: K): ISet
  def (set: ISet) iterator: Iterator[K]
  def (set: ISet) union (other: ISet): ISet = other.iterator.foldLeft(set)(_ insert _)
  def (set: ISet) diff (other: ISet): ISet = other.iterator.foldLeft(set)(_ remove _)
  def (set: ISet) intersect (other: ISet): ISet = set diff (other diff set)
  def emptySet: ISet
}

trait IndexedMap[K]{
  type IMap[+A]

  def (map: IMap[A]) mapMember[A](k: K): Boolean
  def (map: IMap[A]) put[A] (k: K, a: A): IMap[A]
  def (map: IMap[A]) delete[A] (k: K): IMap[A]
  def (map: IMap[A]) get[A] (k: K): Option[A]
  def (map: IMap[A]) keys[A]: Iterator[K]
  def (map: IMap[A]) items[A]: Iterator[(K, A)] = map.keys.flatMap( k =>  map.get(k).map((k, _)).iterator)
  def (map: IMap[A]) putAll[A] (other: IMap[A]): IMap[A] = other.items.foldLeft(map){ case (m, (k, v)) => m put (k, v) }

  def emptyMap[A]: IMap[A]
}



trait IndexedIm[K] extends IndexedSet[K] with IndexedMap[K] {
  def (map: IMap[A]) keySet[A]: ISet
  def (map: IMap[A]) deleteAll[A] (set: ISet) : IMap[A] = set.iterator.foldLeft(map)(_ delete _)
}

trait IndexedSetMut[K]{
  type MSet

  def (set: MSet) setMemberM(k: K): Boolean
  def (set: MSet) insertM (k: K): Unit
  def (set: MSet) removeM (k: K): Unit
  def (set: MSet) iteratorM: Iterator[K]
  def (set: MSet) insertAllM (other: TraversableOnce[K]): Unit = other.foreach(set insertM _)
  def (set: MSet) removeAllM (other: TraversableOnce[K]): Unit = other.foreach(set removeM _)

  def emptyMutSet(): MSet
  def makeSet(ks: TraversableOnce[K]): MSet = {
    val s = emptyMutSet()
    ks.foreach(s insertM _)
    s
  }
}

trait IndexedMapMut[K]{
  type MMap[A]

  def (map: MMap[A]) mapMemberM[A](k: K): Boolean
  def (map: MMap[A]) putM[A] (k: K, a: A): Unit
  def (map: MMap[A]) deleteM[A] (k: K): Unit
  def (map: MMap[A]) getM[A] (k: K): Option[A]
  def (map: MMap[A]) keysM[A]: Iterator[K]
  def (map: MMap[A]) itemsM[A]: Iterator[(K, A)] = map.keysM.flatMap( k =>  map.getM(k).map((k, _)).iterator)
  def (map: MMap[A]) putAllM[A] (other: TraversableOnce[(K, A)]): Unit = other.foreach{ case (k, v) => map putM (k, v) }
  def (map: MMap[A]) deleteAllM[A] (other: TraversableOnce[K]): Unit = other.foreach( map deleteM _ )

  def emptyMutMap[A](): MMap[A]

  def makeMap[A](ks: TraversableOnce[(K, A)]): MMap[A] = {
    val s = emptyMutMap[A]()
    ks.foreach{ case (k, v) => s putM (k, v) }
    s
  }
}

trait IndexedMut[K] extends IndexedSetMut[K] with IndexedMapMut[K]

trait Indexed[K] extends IndexedIm[K] with IndexedMut[K]