package dailytips.chimney
import scala.compiletime.ops.int.*
import scala.compiletime.{constValue, error}
import scala.collection.AbstractMap

type IndexOfRec[T <: Tuple, Name, I <: Int] <: Int = T match
  case EmptyTuple => -1
  case Name *: _  => I
  case _ *: rest  => IndexOfRec[rest, Name, I + 1]

type IndexOf[T <: Tuple, Name] = IndexOfRec[T, Name, 0]

inline def findField[Name, Value, Names <: Tuple, Values <: Tuple](c1: Values): Value =
  inline constValue[IndexOf[Names, Name]] match
    case -1 => error("can't find field" + constValue[Name])
    case i  =>
      inline c1 match
        case ne: NonEmptyTuple =>
          inline ne(i) match
            case v: Value => v
            case _        => error("field")
