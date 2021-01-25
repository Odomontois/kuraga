package playground.typeclasses

object Lolk{
  trait Num[N] {
    extension (x: N)
      def + (y: N): N
      def * (y: N): N
      def - (y: N)   = x + (- y)
    def fromInt(x: Int): N

    def one: N              = fromInt(1)
    def zero: N             = fromInt(0)
    extension (x : N) def unary_- : N = x *  fromInt(-1)
  }

  object Num {
    given Num[Int] with {
      extension (x: Int)
        def + (y: Int) = x + y
        def * (y: Int) = x * y
      def fromInt(x: Int) = x
    }
  }

  def foo[N: Num](x: N) = -x * x

  def main(args: Array[String]): Unit = {
    println(foo(1))
  }

}