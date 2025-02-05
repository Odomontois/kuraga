import scala.deriving.Mirror
import scala.compiletime.*
enum Animal {
    case Cat, Dog, Horse
}

// independent ADT
enum Fruit {
    case Banana, Apple
}

// grouping different entities into few categories
enum Goods[+T] {
    case AnimalCategory[T <: Animal & Singleton](animal: T) extends Goods[T]
    case FruitCategory[T <: Fruit & Singleton](fruit: T)    extends Goods[T]
    case Other
}

enum Box[+T](val category: Goods[T]) {
    case CatBox    extends Box(category = Goods.AnimalCategory(Animal.Cat))
    case DogBox    extends Box(category = Goods.AnimalCategory(Animal.Dog))
    case BananaBox extends Box(category = Goods.FruitCategory(Fruit.Banana))
    case AppleBox  extends Box(category = Goods.FruitCategory(Fruit.Apple))
}

val Animals = summon[Mirror.SumOf[Animal]]
val Boxes   = summon[Mirror.SumOf[Box[Any]]]

type Unbox[T <: Tuple, Q] <: Tuple = T match {
    case EmptyTuple     => EmptyTuple
    case Box[t] *: rest =>
        t match {
            case Q => t *: Unbox[rest, Q]
            case _ => Unbox[rest, Q]
        }
}

1 + 2


val x = constValueTuple[Unbox[Boxes.MirroredElemTypes, Animal]]

// ERROR: no box for AnimalCategory(Animal.Horse)

// trait UnboxAnimals[C <: Coproduct] extends DepFn0 {
//     type Out <: HList
// }

// trait UnboxAnimalsLP {
//     type Aux[C <: Coproduct, O <: HList] = UnboxAnimals[C] { type Out = O }
//     trait Impl[C <: Coproduct, O <: HList] extends UnboxAnimals[C] { type Out = O }
//     protected def impl[C <: Coproduct, O <: HList](x: => O): Impl[C, O] = () => x

//     implicit def caseOther[I, C <: Coproduct](implicit
//         tail: UnboxAnimals[C]
//     ): Aux[I :+: C, tail.Out] = impl(tail())
// }
// object UnboxAnimals extends UnboxAnimalsLP {
//     implicit def caseAnimal[I, A <: Animal, C <: Coproduct](implicit
//         sub: I <:< Box[A],
//         tail: UnboxAnimals[C],
//         w: Witness.Aux[A]
//     ): Aux[I :+: C, A :: tail.Out] = impl(w.value :: tail())

//     implicit val caseCNil: Aux[CNil, HNil]                                        = impl(HNil)
//     def apply[C <: Coproduct](implicit unbox: UnboxAnimals[C]): Aux[C, unbox.Out] = unbox
// }

// val boxes        = Generic[Box[Any]]
// val animals      = Generic[Animal]
// val boxesL       = ops.coproduct.ToHList[boxes.Repr]
// val boxesAnimalL = UnboxAnimals[boxes.Repr]
// val animalsL     = ops.coproduct.ToHList[animals.Repr]
// // boxesAnimalL
// val uncovered    = ops.hlist.Diff[animalsL.Out, boxesAnimalL.Out]
// val remains      = ops.hlist.Reify[uncovered.Out].apply()

// println(remains)

// def CheckCoverage[Bs <: Coproduct, As <: Coproduct, AL <: HList, AB <: HList, Unc <: HList]()(implicit
//     boxes: Generic.Aux[Box[Any], Bs],
//     animals: Generic.Aux[Animal, As],
//     animalsL: ops.coproduct.ToHList.Aux[As, AL],
//     boxesAnimalL: UnboxAnimals.Aux[Bs, AB],
//     uncovered: ops.hlist.Diff.Aux[AL, AB, HNil],
// ) = ()

// CheckCoverage()
