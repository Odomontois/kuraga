package playground.typeclasses
import cats.Parallel
import cats.data.EitherT
import cats.instances.string.*
import cats.instances.list.*

object Foo:
    summon[Parallel[[A] =>> EitherT[List, String, A]]]