import cats.data.NonEmptyMap
import cats.syntax.all.*

val x = NonEmptyMap.of(12 -> 2, 3 -> 7, 1 -> 7)

x.toNel.mkString_("_")
