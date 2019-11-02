package kuraga

trait Forall[F[_]]
    def of[A]: F[A]