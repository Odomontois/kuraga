package kuraga

trait Functor[F[_]]:
    extension [A, B] (fa: F[A]) def map (f: A => B) : F[B]

trait Pure[+F[_]]:
  extension [A] (a: A) def pure : F[A]
  val unit: F[Unit] = ().pure

trait Apply[F[_]] extends Functor[F]:
  extension [A, B, C] (fa: F[A]) def map2 (fb: F[B])(f: (A, B) => C): F[C]

  extension [A, B, C] (fa: Eval[F[A]]) def  map2Lz(fb: Eval[F[B]]) (f: (A, B) => Eval[C]): Eval[F[C]] = 
        for fae <- fa
            fbr <- fb
        yield fae.map2(fbr)((a, b) => f(a, b).value)

trait Applicative[F[_]] extends Pure[F] with Apply[F]:
    extension [A, B] (fa: F[A]) override def map (f: A => B): F[B] = 
        fa.map2(unit)((a, _) => f(a))

trait FlatMap[F[_]] extends Apply[F]:
    extension [A, B] (fa: F[A]) def flatMap(f: A => F[B]): F[B] 

trait FlatMapTail[F[_]] extends FlatMap[F]:
    extension [A, B] (a: A) def  tailRecM(f: A => F[Either[A, B]]): F[B] 

trait Monad[F[_]] extends FlatMapTail[F] with Applicative[F]:
    extension [A, B, C] (fa: F[A]) override def map2 (fb: F[B])(f: (A, B) => C): F[C] = 
        fa.flatMap(a => fb.flatMap(b => f(a, b).pure))   
        

trait StackSafeMonad[F[_]] extends Monad[F]:
    extension [A, B] (a: A) override def tailRecM(f: A => F[Either[A, B]]): F[B] = 
        f(a).flatMap {
            case Left(a)  => a.tailRecM(f)
            case Right(b) => b.pure
        }

trait Extract[F[_]]:
    extension [A](fa: F[A]) def extract: A

trait CoFlatMap[F[_]] extends Functor[F]:
    extension  [A, B](fa: F[A]) def coflatMap (f: F[A] => B) : F[B]

trait Comonad[F[_]] extends CoFlatMap[F] with Extract[F]:
    extension [A, B] (fa: F[A]) def map (f: A => B) : F[B] = fa.coflatMap(fa1 => f(fa1.extract))
