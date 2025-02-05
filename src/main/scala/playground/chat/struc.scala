package playground.chat.struc

trait Foo{
    def xx[A: Numeric](implicit O: Ordering[A]): A
}

object Bar extends Foo{
    def xx[A](implicit N: Numeric[A], O: Ordering[A]): A = ???
}