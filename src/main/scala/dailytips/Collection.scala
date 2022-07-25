package dailytips

trait Collection:
  type Coll[+A]

  def append[A](c: Coll[A], a: A): Coll[A]
  def prepend[A](c: Coll[A], a: A): Coll[A]

  def concat[A](c1: Coll[A], c2: Coll[A]): Coll[A]

end Collection
