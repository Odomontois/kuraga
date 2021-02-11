package kuraga


trait Functor[F[_]]:
    extension [A, B] (fa: F[A]) def map (f: A => B) : F[B]

object Functor extends instances.FunctorInstances


trait Pure[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]
  val unit: F[Unit] = pure(())

trait Apply[F[_]] extends Functor[F]:
  extension [A, B, C] (fa: F[A]) def map2 (fb: F[B])(f: (A, B) => C): F[C]

  extension [A, B, C] (fa: Eval[F[A]]) def  map2Lz(fb: Eval[F[B]]) (f: (A, B) => Eval[C]): Eval[F[C]] = 
        for fae <- fa
            fbr <- fb
        yield fae.map2(fbr)((a, b) => f(a, b).value)

trait Applicative[F[_]] extends Pure[F] with Apply[F]:
    extension [A, B] (fa: F[A]) override def map (f: A => B): F[B] = 
        fa.map2(unit)((a, _) => f(a))

    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = fa.map2(fb)((_, _))

    extension [A, B, C, D](fa: F[A])
        def map3(fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = 
            zip(fa, fb).map2(fc) {
                case ((a, b), c) => f(a, b, c)
            }

    extension [A, B, C, D, E](fa: F[A])
        def map4(fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = 
            zip(fa, fb).map2(zip(fc, fd)){ 
                case ((a, b), (c, d)) => f(a, b, c, d) 
            }


trait FlatMap[F[_]] extends Apply[F]:
    extension [A, B] (fa: F[A]) def flatMap(f: A => F[B]): F[B] 

trait FlatMapTail[F[_]] extends FlatMap[F]:
    extension [A, B] (a: A) def  tailRecM(f: A => F[Either[A, B]]): F[B] 

trait Monad[F[_]] extends FlatMapTail[F] with Applicative[F]:
    extension [A, B, C] (fa: F[A]) override def map2 (fb: F[B])(f: (A, B) => C): F[C] = 
        fa.flatMap(a => fb.flatMap(b => pure(f(a, b))))   
        

trait StackSafeMonad[F[_]] extends Monad[F]:
    extension [A, B] (a: A) override def tailRecM(f: A => F[Either[A, B]]): F[B] = 
        f(a).flatMap {
            case Left(a)  => a.tailRecM(f)
            case Right(b) => pure(b)
        }

trait Extract[F[_]]:
    extension [A](fa: F[A]) def extract: A

trait CoFlatMap[F[_]] extends Functor[F]:
    extension  [A, B](fa: F[A]) def coflatMap (f: F[A] => B) : F[B]

trait Comonad[F[_]] extends CoFlatMap[F] with Extract[F]:
    extension [A, B] (fa: F[A]) def map (f: A => B) : F[B] = fa.coflatMap(fa1 => f(fa1.extract))

trait Traverse[T[_]] extends Functor[T]:
  extension [A, B, F[_]: Applicative](ta: T[A])
    def traverse(f: A => F[B]): F[T[B]]

  extension [A, F[_]: Applicative](ta: T[F[A]])
    def sequence: F[T[A]] = ta.traverse(fa => fa)


