package playground.indexed

trait IndexedSet[K]:
  type ISet

  extension (set: ISet)
    def setMember(k: K): Boolean
    def insert(k: K): ISet
    def remove(k: K): ISet
    def iterator: Iterator[K]
    def union(other: ISet): ISet     = other.iterator.foldLeft(set)(_.insert(_))
    def diff(other: ISet): ISet      = other.iterator.foldLeft(set)(_.remove(_))
    def intersect(other: ISet): ISet = set.diff(other.diff(set))
    def emptySet: ISet

trait IndexedMap[K]:
  type IMap[+A]

  extension [A](map: IMap[A])
    def mapMember(k: K): Boolean
    def put(k: K, a: A): IMap[A]
    def delete(k: K): IMap[A]
    def get(k: K): Option[A]
    def keys: Iterator[K]
    def items: Iterator[(K, A)]         = map.keys.flatMap(k => map.get(k).map((k, _)).iterator)
    def putAll(other: IMap[A]): IMap[A] = other.items.foldLeft(map) { case (m, (k, v)) => m.put(k, v) }

  def emptyMap[A]: IMap[A]

trait IndexedIm[K] extends IndexedSet[K] with IndexedMap[K]:
  extension [A](map: IMap[A])
    def keySet: ISet
    def deleteAll(set: ISet): IMap[A] = set.iterator.foldLeft(map)(_.delete(_))

trait IndexedSetMut[K]:
  type MSet

  extension (set: MSet)
    def setMemberM(k: K): Boolean
    def insertM(k: K): Unit
    def removeM(k: K): Unit
    def iteratorM: Iterator[K]
    def insertAllM(other: IterableOnce[K]): Unit = other.iterator.foreach(set `insertM` _)
    def removeAllM(other: IterableOnce[K]): Unit = other.iterator.foreach(set `removeM` _)

  def emptyMutSet(): MSet
  def makeSet(ks: TraversableOnce[K]): MSet =
    val s = emptyMutSet()
    ks.iterator.foreach(s `insertM` _)
    s

trait IndexedMapMut[K]:
  type MMap[A]

  extension [A](map: MMap[A])
    def mapMemberM(k: K): Boolean
    def putM(k: K, a: A): Unit
    def deleteM(k: K): Unit
    def getM(k: K): Option[A]
    def keysM: Iterator[K]
    def itemsM: Iterator[(K, A)]                      = map.keysM.flatMap(k => map.getM(k).map((k, _)).iterator)
    def putAllM(other: TraversableOnce[(K, A)]): Unit = other.iterator.foreach { case (k, v) => map `putM` (k, v) }
    def deleteAllM(other: TraversableOnce[K]): Unit   = other.iterator.foreach(map `deleteM` _)

  def emptyMutMap[A](): MMap[A]

  def makeMap[A](ks: TraversableOnce[(K, A)]): MMap[A] = {
    val s = emptyMutMap[A]()
    ks.iterator.foreach { case (k, v) => s `putM` (k, v) }
    s
  }

trait IndexedMut[K] extends IndexedSetMut[K] with IndexedMapMut[K]

trait Indexed[K] extends IndexedIm[K] with IndexedMut[K]
