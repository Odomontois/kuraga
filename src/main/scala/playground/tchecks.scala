package playground

object TInfer extends App{
  def foo[T](x: T, f: T => T): T = f(x)

  println(foo(1, x => x + 1))

  def check[C[-_], A, B] = {
    summon[(C[A] & C[B]) =:= C[A | B]]
    summon[(C[A] | C[B]) <:< C[A & B]]
  } 

  def check2[C[+_], A, B] = {
    summon[(C[A] & C[B]) =:= C[A & B]]
    summon[(C[A] | C[B]) <:< C[A | B]]
  } 

  def implem[A, B, R]: Either[A => R, B => R] => ((A, B)) => R =    {
      case Left(f) => ab => f(ab._1)
      case Right(g) => ab => g(ab._2)
    }

    val ho = [A] => (xs: List[A]) => xs.headOption

    println(ho(List(1, 2, 3)))
    println(ho(Nil))
    println(ho("kek".toList))
}


trait PolyLol{
  def kek[A](x: A, y: A): List[A]
}

object PolyLol{

  // val p: PolyLol = [A] => (x: A, y: A) => List(x, y)
}

