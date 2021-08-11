package playground.free

enum HFree[+U[+_[+_], +_], +A]:
    case HPure[+A](a: A) extends HFree[Nothing, A]
    case HBind[+U[+_[+_], +_], A, +B](uf: U[HFree[U, *], A], f: A => HFree[U, B]) extends HFree[U, B]

