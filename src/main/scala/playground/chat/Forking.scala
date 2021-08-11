package playground.chat

final class Forking[+A](mkValue: => A, toLeft: => Forking[A], toRight: => Forking[A]):
  lazy val value = mkValue
  lazy val left  = toLeft
  lazy val right = toRight

  def values: LazyList[A] = LazyList.iterate(BigInt(0))(_ + 1).map(apply)

  def map[B](f: A => B): Forking[B] = Forking(f(value), left.map(f), right.map(f))

  def zipWith[B, C](other: Forking[B])(f: (A, B) => C): Forking[C] =
    Forking(
      f(value, other.value),
      left.zipWith(other.left)(f),
      right.zipWith(other.right)(f)
    )

  def apply(idx: BigInt): A =
    if (idx == 0) value
    else if (idx % 2 == 1) left(idx / 2)
    else right(idx / 2 - 1)

object Forking:
  def repeat[A](value: => A): Forking[A] =
    lazy val res: Forking[A] = Forking(value, res, res)
    res

  lazy val indices: Forking[BigInt] =
    Forking(BigInt(0), indices.map(_ * 2 + 1), indices.map(_ * 2 + 2))

@main def runForking =
  lazy val fibonacci: Forking[BigInt] =
    Forking.indices.map(i => if (i <= 1) i else fibonacci(i - 1) + fibonacci(i - 2))
  println(fibonacci.values.take(100).force)
