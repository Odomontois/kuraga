package playground.typeclasses

trait Functor[F[_]] {
    def [A, B] (fa: F[A]) fmap (f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F]{
    def [A] (a: A) pure : F[A] 
    def pure[A] (a: A)  : F[A] = a.pure
    def [A, B, C] (fa: F[A]) map2 (fb: F[B])(f: (A, B) => C) : F[C]

    def unit: F[Unit] = ().pure
    def [A, B] (ff: F[A => B]) ap (fa: F[A]): F[B] = ff.map2(fa)(_(_))

    override def [A, B] (fa: F[A]) fmap(f: A => B): F[B] = fa.map2(unit)((a, _) => f(a))
}

trait Monad[F[_]] extends Applicative[F] {
    def[A, B] (fa: F[A]) flatMap (f: A => F[B]): F[B]

    override def[A, B, C] (fa: F[A]) map2(fb: F[B])(f: (A, B) => C) = 
      fa.flatMap(a => fb.fmap(b => f(a, b)))
}

trait Traverse[F[_]] extends Functor[F] {
    def[G[_], A, B] (fa: F[A]) traverse (f: A => G[B]) (using Applicative[G]) : G[F[B]]

    def[G[_], A] (fa: F[G[A]]) sequence (using Applicative[G]) : G[F[A]] = fa.traverse(identity)

    override def [A, B] (fa: F[A]) fmap(f: A => B): F[B] = fa.traverse[Identity, A, B](f)
}

trait MonadWithTraverse[F[_]] extends Monad[F] with Traverse[F]
type Identity[A] = A

given idInstance as MonadWithTraverse[Identity] {
    override def [A, B] (fa: A) flatMap (f: A => B) = f(fa)
    override def [A] (a: A) pure = a
    override def [G[_], A, B](a: A) traverse(f: A => G[B]) (using G: Applicative[G]) : G[B] = f(a)
}


given optionInstance as MonadWithTraverse[Option] {
    override def [A, B] (fa: Option[A]) flatMap (f: A => Option[B]) = fa.flatMap(f)
    override def [A] (a: A) pure = Some(a)
    override def [G[_], A, B](a: Option[A]) traverse(f: A => G[B]) (using G: Applicative[G]) : G[Option[B]] = 
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