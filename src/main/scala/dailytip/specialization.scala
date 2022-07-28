package dailytip.specialization

import scala.collection.SortedMap
import scala.collection.immutable.{AbstractMap, IntMap, LongMap}
import compiletime.erasedValue

type OrderedMap[K, +V] = K match
  case Int  => IntMap[V]
  case Long => LongMap[V]
  case _    => SortedMap[K, V]

type Keys[Coll] = Coll match
  case IntMap[v]       => Set[Int]
  case LongMap[v]      => Long
  case SortedMap[k, v] => k

//extension [Coll](map: Coll)
//  inline def keysIterable: Keys[Coll] =
//    inline map match
//      case intMap: IntMap[v] => intMap.keys
//      case _: Long           => map.keys
//      case _                 => map.keys

//private inline def keysIterableMatch[Coll](map: Coll): Keys[Coll] =
//  inline map match
//    case intMap: IntMap[v] => intMap.keySet
