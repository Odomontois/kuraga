package dailytip

import scala.annotation.tailrec

type Init[Coll[_], A, T <: Tuple] = T match
  case EmptyTuple   => A
  case head *: rest => InitCons[Coll, A, head, rest]

type InitCons[Coll[_], A, H, Rest <: Tuple] = H match
  case Int => Init[Coll, Coll[A], Rest]
  case _   => Unit

@tailrec def fillVector[A, T <: Tuple](dims: T)(x: => A): Init[Vector, A, T] =
  dims match
    case _: EmptyTuple                => x
    case head *: rest: (head *: rest) =>
      head match
        case size: Int => fillVector(rest)(Vector.fill(size)(x))
        case _         => ()
