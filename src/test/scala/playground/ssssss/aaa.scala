package playground.ssssss

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.language.dynamics

enum Token:
    case V(name: String)
    case Num(value: Int)
    case Or
    case And

sealed trait Expression:
    def assignments: Seq[Assignment]
    def and(other: Assignment) = Conjunction((assignments :+ other)*)

case class Assignment(name: String, values: Int*) extends Expression:
    def |(value: Int) = Assignment(name, (values :+ value)*)
    def assignments   = Vector(this)

case class Conjunction(assignments: Assignment*) extends Expression

inline def assignment(inline self: Var) = ${ assignmentImpl('self) }

def assignmentImpl(self: Expr[Var])(using q: Quotes): Expr[Assignment] =
    import q.reflect.*
    val name = self.asTerm.underlyingArgument match
        case Ident(name)     => name
        case Select(_, name) => name
        case ts              => report.errorAndAbort(s"tokens: $ts")

    '{ Assignment(${ Expr(name) }) }

class Var:
    inline def ~(value: Int) = assignment(this) | value

object Lol:
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


    // val x = constValueTuple[Unbox[Boxes.MirroredElemTypes, Animal]]
