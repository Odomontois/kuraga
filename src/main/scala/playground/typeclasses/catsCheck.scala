package playground.typeclasses
import cats.Parallel
import cats.data.EitherT
import cats.instances.string._
import cats.instances.list._

object Foo:
    summon[Parallel[[A] =>> EitherT[List, String, A]]]