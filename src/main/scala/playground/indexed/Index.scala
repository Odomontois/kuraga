package playground.indexed

trait IndexedSet[K]:
  type ISet

  def (set: ISet) setMember(k: K): Boolean
  def (set: ISet) insert (k: K): ISet
  def (set: ISet) remove (k: K): ISet
  def (set: ISet) iterator: Iterator[K]
  def (set: ISet) union (other: ISet): ISet = other.iterator.foldLeft(set)(_ insert _)
  def (set: ISet) diff (other: ISet): ISet = other.iterator.foldLeft(set)(_ remove _)
  def (set: ISet) intersect (other: ISet): ISet = set diff (other diff set)
  def emptySet: ISet


trait IndexedMap[K]:
  type IMap[+A]

  def [A] (map: IMap[A]) mapMember(k: K): Boolean
  def [A] (map: IMap[A]) put (k: K, a: A): IMap[A]
  def [A] (map: IMap[A]) delete (k: K): IMap[A]
  def [A] (map: IMap[A]) get (k: K): Option[A]
  def [A] (map: IMap[A]) keys: Iterator[K]
  def [A] (map: IMap[A]) items: Iterator[(K, A)] = map.keys.flatMap( k =>  map.get(k).map((k, _)).iterator)
  def [A] (map: IMap[A]) putAll (other: IMap[A]): IMap[A] = other.items.foldLeft(map){ case (m, (k, v)) => m put (k, v) }

  def emptyMap[A]: IMap[A]



trait IndexedIm[K] extends IndexedSet[K] with IndexedMap[K] :
  def [A] (map: IMap[A]) keySet: ISet
  def [A] (map: IMap[A]) deleteAll (set: ISet) : IMap[A] = set.iterator.foldLeft(map)(_ delete _)


trait IndexedSetMut[K]:
  type MSet

  def (set: MSet) setMemberM(k: K): Boolean
  def (set: MSet) insertM (k: K): Unit
  def (set: MSet) removeM (k: K): Unit
  def (set: MSet) iteratorM: Iterator[K]
  def (set: MSet) insertAllM (other: IterableOnce[K]): Unit = other.iterator.foreach(set insertM _)
  def (set: MSet) removeAllM (other: IterableOnce[K]): Unit = other.iterator.foreach(set removeM _)

  def emptyMutSet(): MSet
  def makeSet(ks: TraversableOnce[K]): MSet = 
    val s = emptyMutSet()
    ks.iterator.foreach(s insertM _)
    s
  


trait IndexedMapMut[K]:
  type MMap[A]

  def [A] (map: MMap[A]) mapMemberM(k: K): Boolean
  def [A] (map: MMap[A]) putM (k: K, a: A): Unit
  def [A] (map: MMap[A]) deleteM (k: K): Unit
  def [A] (map: MMap[A]) getM (k: K): Option[A]
  def [A] (map: MMap[A]) keysM: Iterator[K]
  def [A] (map: MMap[A]) itemsM: Iterator[(K, A)] = map.keysM.flatMap( k =>  map.getM(k).map((k, _)).iterator)
  def [A] (map: MMap[A]) putAllM (other: TraversableOnce[(K, A)]): Unit = other.iterator.foreach{ case (k, v) => map putM (k, v) }
  def [A] (map: MMap[A]) deleteAllM (other: TraversableOnce[K]): Unit = other.iterator.foreach( map deleteM _ )

  def emptyMutMap[A](): MMap[A]

  def makeMap[A](ks: TraversableOnce[(K, A)]): MMap[A] = {
    val s = emptyMutMap[A]()
    ks.iterator.foreach{ case (k, v) => s putM (k, v) }
    s
  }


trait IndexedMut[K] extends IndexedSetMut[K] with IndexedMapMut[K]

trait Indexed[K] extends IndexedIm[K] with IndexedMut[K]