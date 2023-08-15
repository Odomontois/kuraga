import scala.collection.IterableOps
import compiletime.*
import compiletime.ops.int.*

class UnzipGen[F[+_], T <: NonEmptyTuple](xs: F[T])(map: [A, B] => (F[A], A => B) => F[B]):
    inline def run: Tuple.Map[T, F] = step(EmptyTuple, constValue[Tuple.Size[T] - 1])

    private inline def step[Acc <: Tuple, i <: Int](acc: Acc, i: i): Tuple.Map[T, F] =
        inline erasedValue[i >= 0] match
            case _: true  =>
                val ith: F[Tuple.Elem[T, i.type]] = map(xs, (t: T) => t(i))
                step(ith *: acc, constValue[i - 1])
            case _: false => summonInline[Acc =:= Tuple.Map[T, F]](acc)
end UnzipGen

inline def unzipSeq[F[+a] <: IterableOps[a, F, F[a]], T <: NonEmptyTuple](xs: F[T]): Tuple.Map[T, F] =
    UnzipGen[F, T](xs)([A, B] => (fa: F[A], f: A => B) => fa.map(f)).run

val elements = Vector(
  (1, "2", 3.0, List(4), Array(5)),
  (11, "12", 13.0, List(14), Array(15)),
  (21, "22", 23.0, List(24), Array(25)),
)

val (a, b, c, d, e) = unzipSeq(elements)
a
b
c
d
e
