import cats.FlatMap
import cats.SemigroupK
import cats.kernel.Semigroup
import cats.Functor
import cats.Apply

trait Base {
    type K <: AnyKind
    type Requirement[A <: K]
    type Link[A <: K, B <: K]
    type Pair[A <: K, B <: K] <: K
    type One <: K
    def andThen[A <: K: R, B <: K: R, C <: K: R](first: Link[A, B], second: Link[B, C]): Link[A, C]
    def neutral[A <: K: R]: Link[A, A]
    def parallel[A <: K: R, B <: K: R, C <: K: R, D <: K: R](
        left: Link[A, B],
        right: Link[C, D]
    ): Link[Pair[A, C], Pair[B, D]]

    trait Element {
        type T <: K
        def requirement: Requirement[T]
    }

    class ElementOf[ET <: K](val requirement: Requirement[ET]) extends Element {
        type T = ET
    }

    final type R[A <: K] = Requirement[A]
}

object FunctionBase extends Base {
    type K              = Any
    type Requirement[A] = Unit

    type Link[A, B] = A => B
    type Pair[A, B] = (A, B)

    type One = Unit

    def andThen[A, B, C](first: A => B, second: B => C)(using Unit, Unit, Unit): A => C = first.andThen(second)

    def neutral[A](using Unit): A => A = identity

    def parallel[A, B, C, D](left: A => B, right: C => D)(using Unit, Unit, Unit, Unit): ((A, C)) => (B, D) =
        (a, c) => (left(a), right(c))
}

trait HKFunctionBase extends Base {
    type K = [A] =>> Any

    type Link[F[_], G[_]] = [a] => F[a] => G[a]

    override def andThen[F[_]: R, G[_]: R, H[_]: R](first: [a] => (x: F[a]) => G[a], second: [a] => (x: G[a]) => H[a]) =
        [a] => (x: F[a]) => second(first(x))

    def neutral[F[_]: R] = [a] => (x: F[a]) => x

    // just to remind the form of the method
    override def parallel[A[_]: R, B[_]: R, C[_]: R, D[_]: R](
        left: [a] => A[a] => B[a],
        right: [a] => C[a] => D[a]
    ): [a] => Pair[A, C][a] => Pair[B, D][a]
}

object TupleKBase extends HKFunctionBase {
    type Requirement[F[_]] = Unit

    type Pair[F[_], G[_]] = [a] =>> (F[a], G[a])
    type One              = [a] =>> Unit

    def parallel[A[_]: R, B[_]: R, C[_]: R, D[_]: R](
        left: [a] => A[a] => B[a],
        right: [a] => C[a] => D[a]
    ) = [a] =>
        (p: (A[a], C[a])) =>
            val (a, c) = p
            (left(a), right(c))
}

object ComposeBase extends HKFunctionBase {
    type Requirement[F[_]] = Functor[F]

    type Pair[F[_], G[_]] = [a] =>> F[G[a]]

    type One = [a] =>> a

    def parallel[A[_]: Functor, B[_]: Functor, C[_]: Functor, D[_]: Functor](
        left: [a] => (x: A[a]) => B[a],
        right: [a] => (x: C[a]) => D[a]
    ) = [a] => (x: A[C[a]]) => left(Functor[A].map(x)(right(_)))
}

case class Day[F[_], G[_], A, B, C](first: F[A], second: G[B], merge: (A, B) => C)

object ZipBase extends HKFunctionBase {
    type Requirement[F[_]] = Functor[F]

    // a.k.a Day Convolution
    type Pair[F[_], G[_]] = [a] =>> Day[F, G, ?, ?, a]

    override def parallel[A[_]: R, B[_]: R, C[_]: R, D[_]: R](
        left: [a] => (x: A[a]) => B[a],
        right: [a] => (x: C[a]) => D[a]
    ) = [a] => (x: Day[A, C, ?, ?, a]) => Day(left(x.first), right(x.second), x.merge)
}

trait Algebra {
    val base: Base
    val E: base.Element

    import base.*
    final type x[A <: K, B <: K]   = Pair[A, B]
    final type -->[A <: K, B <: K] = Link[A, B]
    final type I                   = One
}

object Algebra {
    trait OfFunctions[A] extends Algebra {
        val base: FunctionBase.type = FunctionBase
        val E: base.ElementOf[A]    = base.ElementOf[A](())

    }

    trait OfTupleK[A[_]] extends Algebra {
        val base: TupleKBase.type = TupleKBase
        val E: base.ElementOf[A]  = base.ElementOf(())
    }

    trait OfCompose[A[_]](using A: Functor[A]) extends Algebra {
        val base: ComposeBase.type = ComposeBase
        val E: base.ElementOf[A]   = base.ElementOf(A)
    }

    trait OfZip[A[_]](using A: Functor[A]) extends Algebra {
        val base: ZipBase.type   = ZipBase
        val E: base.ElementOf[A] = base.ElementOf(A)
    }
}

trait GSemigroup extends Algebra {
    def gcombine: (E.T x E.T) --> E.T

    // def associativity(a: E.T, b: E.T, c: E.T): E.T = ???
}

trait GMonoid extends GSemigroup {
    def gempty: I --> E.T
}

def semigroupTOGSemigroup[A](s: Semigroup[A]) = new GSemigroup with Algebra.OfFunctions[A] {
    def gcombine = s.combine
}

def gsemigroupToSemigroup[A](s: GSemigroup with Algebra.OfFunctions[A]) = new Semigroup[A] {
    def combine(a: A, b: A) = s.gcombine(a, b)
}

def semigroupKToGSemigroup[A[_]](s: SemigroupK[A]) = new GSemigroup with Algebra.OfTupleK[A] {
    def gcombine = [a] => (p: (A[a], A[a])) => s.combineK(p._1, p._2)
}

def gsemigroupToSemigroupK[A[_]](s: GSemigroup with Algebra.OfTupleK[A]) = new SemigroupK[A] {
    def combineK[a](x: A[a], y: A[a]) = s.gcombine((x, y))
}

def flatMapToGSemigroup[F[_]](using F: FlatMap[F]) = new GSemigroup with Algebra.OfCompose[F] {
    def gcombine = [a] => (ffa: (F[F[a]])) => F.flatten(ffa)
}

def gsemigroupToFlatMap[F[_]](using s: GSemigroup with Algebra.OfCompose[F]) =
    given F: Functor[F] = s.E.requirement
    new FlatMap[F] {
        export F.map
        def flatMap[A, B](fa: F[A])(f: A => F[B])         = s.gcombine(F.map(fa)(f))
        def tailRecM[a, b](a: a)(f: a => F[Either[a, b]]) = ???
    }
end gsemigroupToFlatMap

def applyToGSemigroup[F[_]](using F: Apply[F]) = new GSemigroup with Algebra.OfZip[F] {
    def gcombine = [a] => (p: Day[F, F, ?, ?, a]) => F.map2(p.first, p.second)(p.merge)
}

def gsemigroupToApply[F[_]](using s: GSemigroup with Algebra.OfZip[F]) =
    given F: Functor[F] = s.E.requirement
    new Apply[F] {
        export F.map
        def ap[A, B](ff: F[A => B])(fa: F[A]) = s.gcombine(Day(ff, fa, (f, a) => f(a)))
    }

end gsemigroupToApply

/** | Сочетание | Требование | Ассоциативный тайпкласс | Тайпкласс с нейтральным элементом |
  * |:----------|:-----------|:------------------------|:----------------------------------|
  * | Tuple2    | ----       | Semigroup               | Monoid                            |
  * | Tuple2K   | ----       | SemigroupK              | MonoidK                           |
  * | Nested    | Functor    | FlatMap                 | Monad                             |
  * | Day       | Functor    | Apply                   | Applicative                       |
  */
