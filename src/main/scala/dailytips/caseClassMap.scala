package dailytips.caseClassMap

import scala.deriving.Mirror
import scala.compiletime.*

case class Minsc(
    strength: 18,
    dexterity: 15,
    constitution: 15,
    intellect: 8,
    wisdom: 6,
    charisma: 9,
) derives ValidatedStats

trait ValidatedStats[T]:
    def asMap: Map[String, Int]

object ValidatedStats:
    private inline def validatePair[X <: (String, Int)](): Unit =
        inline erasedValue[X] match
            case _: (name, value) =>
                inline constValue[value] match
                    case x if x < 6 || x > 18 =>
                        error("Invalid value for stat " + constValue[name] + ": " + codeOf(constValue[value]))
                    case x                    =>

    private inline def validateStats[NT <: Tuple](inline nt: NT, inline xs: List[(String, Int)]): Map[String, Int] =
        inline nt match
            case nt1: (e *: ts)  =>
                inline nt1.head match
                    case h: (String, Int & Singleton) =>
                        validatePair[h.type]()
                        validateStats(nt1.tail, h :: xs)
                    case n: (name, _)                 =>
                        error("stat " + constValue[name] + " should be a constant integer")
            case _: EmptyTuple => xs.toMap

    inline def derived[T](using ev: Mirror.ProductOf[T]): ValidatedStats[T] =
        new:
            val labels = constValueTuple[ev.MirroredElemLabels]
            val values = constValueTuple[ev.MirroredElemTypes]
            val asMap = validateStats(labels.zip(values), Nil)
end ValidatedStats


@main def ccmap() = 
    println(summon[ValidatedStats[Minsc]].asMap)