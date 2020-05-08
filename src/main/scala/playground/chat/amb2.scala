package playground.chat

import cats._
import cats.implicits.{given _}

def [F[_]: Monad, A, B] (fa: F[A]) boom (f: A => List[F[B]]): Alternative[F] ?=> F[Unit] = 
    fa.flatMap(a => f(a).foldK.void)

@main def check = Vector(1) boom (i => List(Vector(2), Vector(3)))