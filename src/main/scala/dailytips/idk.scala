package dailytips.idk

trait Semigroup[A]:
  extension (x: A) def |+|(y: A): A

given Semigroup[Int] with
  extension (x: Int) def |+|(y: Int) = x + y
