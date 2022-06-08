package kuraga
package hkdtalk

import cats._
import cats.data.EitherNel
import cats.effect.IO
import cats.effect.Ref

type Stream[F[_], A]

type Tag = String
type Encoder[_]
type Decoder[_]
type Swagger[_]

case class Person(
    firstName: String,
    lastName: Option[String],
    age: Int,
    tags: List[Tag],
)

val person = Person(
    firstName = "レグ",
    lastName = None,
    age = 214,
    tags = List("robot", "indestructible")
)

case class PartialPerson(
    firstName: Option[String],
    lastName: Option[Option[String]],
    age: Option[Int],
    tags: Option[List[Tag]],
) {
    def toPerson: EitherNel[String, Person] = ???
}

given Monoid[PartialPerson] = ???

given Decoder[PartialPerson] = ???
given Encoder[PartialPerson] = ???
given Swagger[PartialPerson] = ???


case class PersonVariants(
    firstName: List[String],
    lastName: List[Option[String]],
    age: List[Int],
    tags: List[List[Tag]],
)

case class MutablePerson(
    firstName: Ref[IO, String],
    lastName: Ref[IO, Option[String]],
    age: Ref[IO, Int],
    tags: Ref[IO, List[Tag]],
)


case class PersonChanges(
    firstName: Stream[IO, String],
    lastName: Stream[IO, Option[String]],
    age: Stream[IO, Int],
    tags: Stream[IO, List[Tag]],
)

case class PersonOf[F[_]](
    firstName: F[String],
    lastName: F[Option[String]],
    age: F[Int],
    tags: F[List[Tag]],
)


type Listen[-A] = (A, A) => IO[Unit]
case class PersonListen(
    firstName: Listen[String],
    lastName: Listen[Option[String]],
    age: Listen[Int],
    tags: Listen[List[Tag]],
)


object PersonOf:
    type Person = PersonOf[Id]
    type PartialPerson = PersonOf[Option]
    type PersonVariants = PersonOf[List]
    type MutablePerson[F[_]] = PersonOf[Ref[F, *]]
    type PersonChanges[F[_]] = PersonOf[Stream[F, *]]
    type PersonListen[F[_]] = PersonOf[Listen]




