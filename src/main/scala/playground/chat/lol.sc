def foo[F[+_], A, B] = summon[F[A] | F[B] <:< F[A | B]]


val zo = 1