package playground.mega

final class Result
final class Error
final class Context
final class Input
final class Output


type MonoT = [_ <: Result] =>> Any
type BiT = [_ <: Result | Error] =>> Any 
type ProT = [_ <: Result | Context] =>> Any
type ReaT = [_ <: Result | Error | Context] =>> Any

type Mono = [T[a <: Result]] =>> Any
type Bi = [T[a <: Result]] =>> Any
type Pro = [T[a <: Result | Context]] =>> Any
type Rea = [T[a <: Result | Error | Context]] =>> Any

type M[F[_[_ <: Result]], A] = F[[T <: Result] =>> T match {
    case Result => A
}]

type BiA[F[_[_ <: Result | Error]], E, A] = F[[T <: Result | Error] =>> T match { 
    case Error => E
    case Result => A
}]

type ProA[F[_[_ <: Result | Context]], R, A] = F[[T <: Context | Result] =>> T match {
    case Context => R
    case Result => A
}]

type Reap[F[_[_ <: Context | Error | Result]], R, E, A] = F[[T <: Context | Error | Result] =>> T match {
    case Context => R
    case Result => A
    case Error => E
}]

type EitherR[T[_ <: Error | Result]] = Either[T[Error], T[Result]]
type FunctionR[T[_ <: Context | Result]] = T[Context] => T[Result]


trait Monad[F[_[_ <: Result]]]:
    def [A](a: A) pure: M[F, A]
    def [A, B](fa: M[F, A]) flatMap (f: A => M[F, B]): M[F, B]

trait BIOError[F[_[_ <: Error | Result]]]:
    def [E, A] (e: E) raise: BiA[F, E, A]
    def [E, A](fa: BiA[F, E, A]) handleWith(f: E => BiA[F, Nothing, A]) : BiA[F, Nothing, A]

trait ProLocal[F[_[_ <: Context | Result]]]:
    def ask[R] : ProA[F, R, R]
    def[R, R1, A](fa: ProA[F, R1, A]) local (f: R => R1): ProA[F, R, A]

final case class ApplicationContext()



// def mapThree[F <: Rea](fs: Reap[F, ApplicationContext, Throwable, String]) (using BIOError[F], ProLocal[F], Monad[F]): Reap[F, Any, Nothing, String] = 
//     summon[ProLocal[F]].local[A = String](fs)(_ => ApplicationContext()).handleWith(es => es.pure)



