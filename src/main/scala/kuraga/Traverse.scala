package kuraga

trait Traversing[C, A]
    def[F[_], B] (c: C) traverse(f: A => F[B])(given Applicative[F]): F[C]