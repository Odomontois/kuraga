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

type Mono = [T <: MonoT] =>> Any
type Bi = [T <: BiT] =>> Any
type Pro = [T <: ProT] =>> Any
type Rea = [T <: ReaT] =>> Any

type M[F <: Mono, A] = F[[T <: Result] =>> T match {
    case Result => A
}]

type BiA[F <: Bi, E, A] = F[[T <: Result | Error] =>> T match { 
    case Error => E
    case Result => A
}]

type ProA[F <: Pro, R, A] = F[[T <: Context | Result] =>> T match {
    case Context => R
    case Result => A
}]

type Reap[F <: Rea, R, E, A] = F[[T <: Context | Error | Result] =>> T match {
    case Context => R
    case Result => A
    case Error => E
}]

type EitherR[T <: BiT] = Either[T[Error], T[Result]]
type FunctionR[T <: ProT] = T[Context] => T[Result]


trait Monad[F <: Mono]:
    def [A](a: A) pure: M[F, A]
    def [A, B](fa: M[F, A]) flatMap (f: A => M[F, B]): M[F, B]

trait BIOError[F <: Bi]:
    def [E, A] (e: E) raise: BiA[F, E, A]
    def [E, A](fa: BiA[F, E, A]) handleWith(f: E => BiA[F, Nothing, A]) : BiA[F, Nothing, A]

trait ProLocal[F <: Pro]:
    def ask[R] : ProA[F, R, R]
    def[R, R1, A](fa: ProA[F, R1, A]) local (f: R => R1): ProA[F, R, A]

final case class ApplicationContext()



// def mapThree[F <: Rea](fs: Reap[F, ApplicationContext, Throwable, String]) (using BIOError[F], ProLocal[F], Monad[F]): Reap[F, Any, Nothing, String] = 
//     summon[ProLocal[F]].local[A = String](fs)(_ => ApplicationContext()).handleWith(es => es.pure)



