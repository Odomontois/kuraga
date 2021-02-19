package playground.typeclasses

trait Functor[F[_]] {
    extension [A, B] (fa: F[A]) def fmap (f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F]{
    extension [A] (a: A) def pure : F[A] 
    extension [A, B, C] (fa: F[A]) def map2 (fb: F[B])(f: (A, B) => C) : F[C]

    def unit: F[Unit] = ().pure
    extension [A, B] (ff: F[A => B]) def ap (fa: F[A]): F[B] = ff.map2(fa)(_(_))

    extension [A, B] (fa: F[A]) override def fmap(f: A => B): F[B] = fa.map2(unit)((a, _) => f(a))
}

trait Monad[F[_]] extends Applicative[F] {
    extension [A, B] (fa: F[A]) def flatMap (f: A => F[B]): F[B]

    extension [A, B, C] (fa: F[A]) override def map2(fb: F[B])(f: (A, B) => C) = 
      fa.flatMap(a => fb.fmap(b => f(a, b)))
}

trait Traverse[F[_]] extends Functor[F] {
    extension [A] (fa: F[A])  def traverse[G[_], B] (f: A => G[B]) (using Applicative[G]) : G[F[B]]

    extension [G[_], A] (fa: F[G[A]]) def sequence (using Applicative[G]) : G[F[A]] = fa.traverse(identity)

    extension [A, B] (fa: F[A]) override def fmap(f: A => B): F[B] = 
        fa.traverse[Identity, B](f)
}

trait MonadWithTraverse[F[_]] extends Monad[F] with Traverse[F]
type Identity[A] = A

given idInstance :  MonadWithTraverse[Identity] with {
    extension [A, B] (fa: A) override def flatMap (f: A => B) = f(fa)
    extension  [A] (a: A) override def pure = a
    extension [A] (a: A) override def traverse[G[_], B](f: A => G[B]) (using G: Applicative[G]) : G[B] = f(a)
}


given optionInstance :  MonadWithTraverse[Option] with {
    extension [A, B] (fa: Option[A]) override def flatMap (f: A => Option[B]) = fa.flatMap(f)
    extension [A] (a: A) override def pure = Some(a)
    extension [A] (a: Option[A]) override def traverse[G[_], B](f: A => G[B]) (using G: Applicative[G]) : G[Option[B]] = 
       a.fold(G.pure(None))(a => f(a).fmap(Some(_)))
}

// ambigous
// def foo[A, B, F[_]: Monad: Traverse](x: F[A], f: A => B): F[B] = x.fmap(f) 

// ambigous
// def foo[A, B, F[_]](x: F[A], f: A => B) given Monad[F] given Traverse[F] : F[B] = x.fmap(f) 

// good
def foo[A, B, F[_]](x: F[A], f: A => B) :  Monad[F] ?=> Traverse[F] ?=> F[B] = x.fmap(f) 

def foo2[A, B, F[_]](x: F[A], f: A => B) (using Monad[F], Traverse[F]) : F[B] = {
    given Functor[F] = summon[Monad[F]]
    x.fmap(f) 
}

extension (i: Int) {
    def lol = i * 2 + 3
}

object KekKekek{
   def main(args: Array[String]) =  println(4.lol)
}