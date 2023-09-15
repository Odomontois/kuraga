enum Sub[-A, +B]:
    case X[T >: A <: B, A <: B, B]() extends Sub[A, B]

object Sub:
    def of[A, B](using sub: Sub[A, B]): Sub[A, B] = sub
    given ss[A, B](using neq: A <:< B): Sub[A, B] =
        neq.liftCo[Sub[A, *]](Sub.X())

def foo[A, B, C, D](x: Either[A, C])(using A Sub B, C Sub D): Either[B, D] =
    (Sub.of[A, B], Sub.of[C, D]) match
        case (Sub.X(), Sub.X()) => x

2 + 2
