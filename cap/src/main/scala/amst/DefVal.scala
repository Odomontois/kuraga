package amst

import scala.caps.Mutable
import scala.annotation.unroll

abstract class Kek[A]

abstract class Lol[A <: Any^] {
  def lol: A^{this}
}

class Cheburek extends Mutable

def check() = {

  def foo[A <: Int | String](x: A) =
    x match
      case 1         => ()
      case _: String => ()

  def bar(x: Int | String) = x match
    case 1         =>
    case _: String =>
}

final def lol(k: String, @unroll u: Int = 0) = 2