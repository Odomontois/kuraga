import playground.day._
import cats._

summon[Functor[DayF[Id, Id]]]

summon[Applicative[DayF[Id, Id]]]

summon[Comonad[DayF[Id, Id]]]

