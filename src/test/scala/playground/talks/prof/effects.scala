package playground.talks.prof.effects

import playground.talks.prof.ProTraverseRep

case class Dictionary(selectDynamic: Map[String, Any]) extends Selectable:
  type Underlying
  def as[T]: Dictionary & T { type Underlying <: T } = this.asInstanceOf[Dictionary & T & { type Underlying <: T }]

  infix def merge[Q](other: Dictionary & Q) =
    Dictionary(selectDynamic ++ other.selectDynamic).as[Underlying & Q]

type Person = {
  val name: String
  val age: Int
}

val x = Dictionary(Map("age" -> 87, "name" -> "Aragorn")).as[Person]

type Friendly = {
  val friends: List[String]
}

val y = Dictionary(Map("friends" -> List("Frodo", "Legolas", "Gimli"))).as[Friendly]

@main def foo() =
  println(x.age: Int)
  println(x.name: String)

  val z = x merge y

  println(z.age: Int)
  println(z.name: String)
  println(z.friends: List[String])
end foo

case class Pro[P[-_, +_], -I, +O](p: P[I, O], instance: ProTraverseRep[P])

trait OfBool[-I, +O] derives ProTraverseRep:
  def True: O
  def False: O
  def Not(x: I): O
  def And(x: I, y: I): O
  def Or(x: I, y: I): O

type Bools[-I, +O] =
  Multi[I, O] & {
    val bools: Pro[OfBool, I, O]
  }

trait OfNumber[-I, +O] derives ProTraverseRep:
  def fromInt(x: Int): O
  def plus(x: I, y: I): O
  def multiply(x: I, y: I): O

type Numbers[-I, +O] =
  Multi[I, O] & {
    val numbers: Pro[OfNumber, I, O]
  }

trait OfCompare[-I, +O] derives ProTraverseRep:
  def less(x: I, y: I): O
  def equals(x: I, y: I): O

type Compare[-I, +O] =
  Multi[I, O] & {
    val comparison: OfCompare[I, O]
  }

case class Multi[-I, +O](pros: Map[String, Pro[?, I, O]]) extends Selectable:
  def selectDynamic(name: String): Pro[?, I, O] = pros(name)

type &&&[P[-_, +_], Q[-_, +_]] = [a, b] =>> P[a, b] & Q[a, b]

trait Layer[-P[-_, +_]]:
  def unwrap[O](p: P[Layer[P], O]): O

trait Handler[From[-_, +_], -To[-_, +_]]:
  def handle[P[-i, +o]]: From[ProData[P &&& From], ProData[P &&& To]]

enum ProData[-P[-_, +_]]:
  case Construct(layer: Layer[P])
  case Eliminate[-P[-_, +_], Q[-_, +_]](
      data: ProData[P &&& Q],
      handler: Handler[Q, P]
  ) extends ProData[P]

object Handler:
  def recursive[From[-i, +o], To[-_, +_]](handler: Handler[From, From &&& To]): Handler[From, To] = ???
