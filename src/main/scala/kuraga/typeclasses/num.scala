package kuraga.typeclasses

object Lolk{
  trait Num[N] {
    def (x: N) + (y: N): N
    def (x: N) * (y: N): N
    def fromInt(x: Int): N

    def one: N              = fromInt(1)
    def zero: N             = fromInt(0)
    def (x : N) unary_- : N = x *  fromInt(-1)
    def (x: N) - (y: N)   = x + (- y)
  }

  object Num {
    given Num[Int]{
      def (x: Int) + (y: Int) = x + y
      def (x: Int) * (y: Int) = x * y
      def fromInt(x: Int) = x
    }
  }

  def foo[N: Num](x: N) = -x * x

  def main(args: Array[String]): Unit = {
    println(foo(1))
  }
}