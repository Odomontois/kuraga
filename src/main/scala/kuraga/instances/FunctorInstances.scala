package kuraga
package instances

trait FunctorInstances:
  given Monad[List] with
    def pure[A](a: A): List[A]                                          = List(a)
    extension [A, B](fa: List[A]) def flatMap(f: A => List[B]): List[B] = fa.flatMap(f)
    extension [A, B](a: A)
      def tailRecM(f: A => List[Either[A, B]]): List[B] =
        def go(stack: List[List[A]], acc: List[List[B]]): List[B] = stack match
          case Nil :: rest            => go(rest, acc)
          case (head :: tail) :: rest =>
            val (as, bs) = f(head).partitionMap(identity)
            go(as :: tail :: rest, bs :: acc)
          case Nil                    => acc.reverse.flatten
        go(List(List(a)), Nil)

  given Monad[Identity] with
    def pure[A](a: A): A                              = a
    extension [A, B](fa: A) def flatMap(f: A => B): B = f(fa)
    extension [A, B](a: A)
      def tailRecM(f: A => Either[A, B]): B           =
        f(a) match
          case Left(a1) => a1.tailRecM(f)
          case Right(b) => b
