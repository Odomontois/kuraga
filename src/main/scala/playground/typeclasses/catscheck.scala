package playground.typeclasses
import cats.Parallel
import cats.data.EitherT

object Foo:
    summon[Parallel[[A] =>> EitherT[List, String, A]]]