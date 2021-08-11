package playground.chat

case class SpecV[@specialized(Int, Double) A](value: A)

trait Plus[@specialized(Int, Double) A] {
  extension (x: A) def plus(y: A): A
}

class Speco[@specialized(Int, Double) A](using Plus[A]) {
  def plus(x: SpecV[A], y: SpecV[A]): SpecV[A] = SpecV(x.value.plus(y.value))
}

def gogo(s: Speco[Long], x: SpecV[Long], y: SpecV[Long]): SpecV[Long] =
  s.plus(x, y)
