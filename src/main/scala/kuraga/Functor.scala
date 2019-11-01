package kuraga

trait Functor[F[_]]
    def [A, B] (fa: F[A]) map (f: A => B) : F[B]


trait Pure[+F[_]]
    def [A] (a: A) pure : F[A]
    val unit: F[Unit] = ().pure

trait Apply[F[_]] extends Functor[F]
    def [A, B, C] (fa: F[A]) map2 (fb: F[B])(f: (A, B) => C): F[C]

trait Applicative[F[_]] extends Pure[F] with Apply[F]
    override def [A, B] (fa: F[A]) map(f: A => B): F[B] = 
        fa.map2(unit)((a, _) => f(a))

trait FlatMap[F[_]] extends Apply[F]
    def [A, B] (fa: F[A]) flatMap(f: A => F[B]): F[B] 

trait FlatMapTail[F[_]] extends FlatMap[F]
    def [A, B] (a: A) tailRecM(f: A => F[Either[A, B]]): F[B]

trait Monad[F[_]] extends FlatMapTail[F] with Applicative[F]
    override def [A, B, C] (fa: F[A]) map2(fb: F[B])(f: (A, B) => C): F[C] = 
        fa.flatMap(a => fb.flatMap(b => f(a, b).pure))   

trait StackSafeMonad[F[_]] extends Monad[F]
    override def [A, B] (a: A) tailRecM (f: A => F[Either[A, B]]): F[B] = 
        f(a).flatMap {
            case Left(a)  => a.tailRecM(f)
            case Right(b) => b.pure
        }