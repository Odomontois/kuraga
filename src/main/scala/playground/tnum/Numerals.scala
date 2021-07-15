package playground.tnum
import scala.compiletime.erasedValue
import scala.annotation.tailrec

object Numerals:
  sealed class Num
  final class Zero           extends Num
  final class Succ[n <: Num] extends Num

  type One = Succ[Zero]
  type Two = Succ[One]

  infix type +[X <: Num, Y <: Num] <: Num = X match
    case Zero    => Y
    case Succ[x] => x + Succ[Y]

  infix type *[X <: Num, Y <: Num] <: Num = X match
    case Zero    => Zero
    case Succ[x] => Y + (x * Y)

  inline def toInt[X <: Num]: Int = inline erasedValue[X] match
    case _: Zero    => 0
    case _: Succ[x] => toInt[x] + 1

  inline def toIntR[X <: Num](acc: Int = 0): Int = inline erasedValue[X] match
    case _: Zero    => acc
    case _: Succ[x] => toIntR[x](acc + 1)

  type Five                               = Two + Two + One
  type Ten                                = Two * Five
  type Hundred                            = Ten * Ten

import Numerals._

@main def numeralsMain() =
  println(toIntR[Ten * (Ten + Five)]())
