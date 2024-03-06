import playground.mega.M
import scala.compiletime.ops.int.*

// trait FindKey[M, K]:
//     type Depth <: Int
//     type Value
//     def get(m: M, key: K): Value

// trait LowPrioFind:
//     given [K, H, D <: Int, T <: Tuple, V](using tail: HasKey[T, D, K, V]): HasKey[H *: T, D + 1, K, V] with
//         def get(m: H *: T, key: K): V = tail.get(m.tail, key)

// object FindKey extends LowPrioFind:
//     given [K, V]: HasKey[Map[K, V], 0, K, V] with
//         def get(m: Map[K, V], key: K): V = m(key)

//     given [K]: HasKey[EmptyTuple, 0, K, Unit] with
//         def get(m: EmptyTuple, key: K): Unit = ()

//     given [K, H, T <: Tuple, V](using head: FindKey[H, K] { type Value = V }): HasKey[H *: T, 0, K, V] with
//         def get(m: H *: T, key: K): V = head.get(m.head, key)
// end FindKey

// trait HasKey[M, D <: Int, K, V] extends FindKey[M, K]:
//     type Depth = D
//     type Value = V

// val maps = (Map(1 -> '2'), Map("one" -> 2), Map('1' -> "two"))

// extension [M](map: M)
//     def get[K, D <: Int, V](k: K)(using hk: HasKey[M, D, K, V]) /* (using ValueOf[hk.Depth]) */: V =
//         // println(s"Depth: ${valueOf[hk.Depth]}")
//         hk.get(map, k)

// maps.get(1)
// maps.get("one")
// maps.get('1')

import scala.compiletime.ops.int.*

type Min[N <: Int, M <: Int] <: Int = (N < M) match
    case true => N
    case false => M

object NVector:

    type NVector[N <: Int, +A] <: Vector[A] {
        def length: N
    }
    val empty: NVector[0, Nothing] = Vector.empty.asInstanceOf[NVector[0, Nothing]]

    extension [N <: Int, A](xs: NVector[N, A])
        def :!+[B](x: B): NVector[N + 1, A | B] = (x +: xs).asInstanceOf[NVector[N + 1, A | B]]
        def zipN[M <: Int, B](ys: NVector[M, B]): NVector[Min[N, M], (A, B)] = xs.zip(ys).asInstanceOf[NVector[Min[N, M], (A, B)]]
end NVector

type NVector[N <: Int, +A] = NVector.NVector[N, A]

val xs = NVector.empty :!+ 1 :!+ 2 :!+ 3

val ys = NVector.empty :!+ "one" :!+ "two"

xs.zipN(ys)