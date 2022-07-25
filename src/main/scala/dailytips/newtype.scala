package dailytips

object Reverse:
  opaque type Reverse[A] = A

  def apply[A](a: A): Reverse[A] = a

  given [A](using A: Ordering[A]): Ordering[Reverse[A]] with
    def compare(x: Reverse[A], y: Reverse[A]) = A.compare(y, x)
end Reverse

@main def foo() =
  println(List(5, 3, 1, 7, 9, 2).sortBy(Reverse(_))) // List(9, 7, 5, 3, 2, 1)
