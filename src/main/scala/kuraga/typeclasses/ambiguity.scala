package kuraga.typeclasses

trait Functor[F[_]] {
    def (fa: F[A]) fmap[A, B] (f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F]{
    def (a: A) pure[A] : F[A]
    def (fa: F[A]) map2[A, B, C] (fb: F[B])(f: (A, B) => C) : F[C]

    def unit: F[Unit] = ().pure
    def (ff: F[A => B]) ap[A, B](fa: F[A]): F[B] = ff.map2(fa)(_(_))

    override def (fa: F[A]) fmap[A, B](f: A => B): F[B] = fa.map2(unit)((a, _) => f(a))
}

trait Monad[F[_]] extends Applicative[F] {
    def (fa: F[A]) flatMap[A, B] (f: A => F[B]): F[B]

    override def (fa: F[A]) map2[A, B, C](fb: F[B])(f: (A, B) => C) = 
      fa.flatMap(a => fb.fmap(b => f(a, b)))
}

trait Traverse[F[_]] extends Functor[F] {
    def (fa: F[A]) traverse[G[_], A, B] (f: A => G[B]) given Applicative[G] : G[F[B]]

    def (fa: F[G[A]]) sequence[G[_], A] given Applicative[G] : G[F[A]] = fa.traverse(identity)

    override def (fa: F[A]) fmap[A, B](f: A => B): F[B] = fa.traverse[Identity, A, B](f)
}

trait MonadWithTraverse[F[_]] extends Monad[F] with Traverse[F]
type Identity[A] = A

given idInstance as MonadWithTraverse[Identity] {
    override def (fa: A) flatMap[A, B] (f: A => B) = f(fa)
    override def (a: A)  pure[A] = a
    override def (a: A)  traverse[G[_], A, B](f: A => G[B]) given (G: Applicative[G]) : G[B] = f(a)
}


given optionInstance as MonadWithTraverse[Option] {
    override def (fa: Option[A]) flatMap[A, B] (f: A => Option[B]) = fa.flatMap(f)
    override def (a: A) pure[A] = Some(a)
    override def (a: Option[A]) traverse[G[_], A, B](f: A => G[B]) given (G: Applicative[G]) : G[Option[B]] = 
       a.fold(G.pure(None))(a => f(a).fmap(Some(_)))
}

// ambigous
// def foo[A, B, F[_]: Monad: Traverse](x: F[A], f: A => B): F[B] = x.fmap(f) 

// ambigous
// def foo[A, B, F[_]](x: F[A], f: A => B) given Monad[F] given Traverse[F] : F[B] = x.fmap(f) 

// good
def foo[A, B, F[_]](x: F[A], f: A => B) : given Monad[F] => given Traverse[F] => F[B] = x.fmap(f) 

def foo2[A, B, F[_]](x: F[A], f: A => B) given Monad[F] given Traverse[F] : F[B] = {
    given as Functor[F] = the[Monad[F]]
    x.fmap(f) 
}

given intOps (i: Int){
    def lol = i * 2 + 3
}

object KekKekek{
   def main(args: Array[String]) =  println(4.lol)
}