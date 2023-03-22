import scala.compiletime.ops.int.<
import scala.compiletime.ops.string.{+, Matches}
import scala.compiletime.constValueTuple

type SortInsert[A, T, Lt[_, _] <: Boolean] <: Tuple = T match 
    case b *: as => Lt[A, b] match 
        case true => A *: SortInsert[b, as, Lt]
        case false => b *: SortInsert[A, as, Lt]
    case EmptyTuple => A *: EmptyTuple

type Sort[T <: Tuple, Lt[_, _] <: Boolean] = T match 
    case a *: as => SortInsert[a, Sort[as, Lt], Lt]
    case EmptyTuple => EmptyTuple

type UUU[A, B] <: Boolean = (A, B) match
    case (Int, String) => true
    case (String, Int) => false
    case (Int, Int) => A < B
    case (String, String) => Matches[B, A + ".*"]

type XXX = (3, "ab", 1, "abc", 2, "a")

println(constValueTuple[Sort[XXX, UUU]])

