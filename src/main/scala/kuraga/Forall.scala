package kuraga

trait Forall[+T[_]]:
  def of[A]: T[A]

trait Forall2[+T[_, _]]:
  def of[A, B]: T[A, B]
