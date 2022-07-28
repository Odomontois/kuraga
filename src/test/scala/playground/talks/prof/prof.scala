package playground.talks.prof

import kuraga.{Applicative, Eval, Id, Identity}

trait FromUser[-I, +O]:
  def anonymous: O
  def authorized(
      name: String,
      email: String,
      friends: Vector[I]
  ): O
  def admin(key: Seq[Byte]): O

trait WrappedUser:
  def patMat[A](f: UserF[WrappedUser] => A): A

enum UserF[+R]:
  case Anonymous
  case Authorized(
      name: String,
      email: String,
      friends: Vector[R]
  )
  case Admin(key: Seq[Byte])

final case class Fix[+F[+_]](value: F[Fix[F]])

type U1 = UserF[WrappedUser]

type U2 = Fix[UserF]

//trait Layer[-P[-_, +_]]:
//  def unwrap[A](f: P[Layer[P], A]): A

trait Wrapped[+F[+_]]:
  def patMat[A](f: F[Wrapped[F]] => A): A

type UUU[+F[+_]] =
  F[Wrapped[F]]

trait ProRep[P[_, _], A]:
  def apply[B](p: P[A, B]): B

//trait ProRepresentable[P[_, _]]:
//  type F[_]
//  def tabulate[A, B](f: F[A] => B): P[A, B]
//  def index[A, B](p: P[A, B])(fa: F[A]): B

trait ProRepresentable[P[_, _]]:
  def tabulate[A, B](f: ProRep[P, A] => B): P[A, B]

trait ProLeftMap[P[_, _]]:
  def leftMap[A, B, L](pab: P[A, B])(f: L => A): P[L, B]

  extension [A, B](pab: P[A, B]) def lmap[L](f: L => A): P[L, B] = leftMap(pab)(f)

trait ProSequence[P[_, _]]:
  def sequence[A, B, F[_]: Applicative](pab: P[A, B]): P[F[A], F[B]]

  extension [A, B](pab: P[A, B]) def seq[F[_]: Applicative]: P[F[A], F[B]] = sequence(pab)

trait ProTraverseRep[P[_, _]] extends ProRepresentable[P] with ProLeftMap[P] with ProSequence[P]:
  def proTraverse[L, A, B, F[_]: Applicative](
      fromRep: ProRep[P, A] => B,
      f: L => A
  ): P[F[L], F[B]]

  override def leftMap[A, B, L](pab: P[A, B])(f: L => A): P[L, B] =
    proTraverse[L, A, B, [x] =>> x](_(pab), f)

  override def sequence[A, B, F[_]: Applicative](pab: P[A, B]): P[F[A], F[B]] =
    proTraverse[A, A, B, F](_(pab), identity)

  override def tabulate[A, B](f: ProRep[P, A] => B): P[A, B] =
    proTraverse[A, A, B, [x] =>> x](f, identity)
end ProTraverseRep

object ProTraverseRep:
  inline def derived[P[-_, +_]]: ProTraverseRep[P] = ???

trait Layer[-P[-_, +_]]:
  def unwrap[A](f: P[Layer[P], A]): A

  def fold[A, P1[x, y] <: P[x, y]](f: P1[A, A])(using ProLeftMap[P1]): A =
    unwrap(f.lmap[Layer[P]](_.fold(f)))

  def foldEval[A, P1[x, y] <: P[x, y]](f: P1[Eval[A], Eval[A]])(using ProLeftMap[P1]): Eval[A] =
    unwrap(f.lmap(_.foldEval(f))).defer

  def foldTail[A, P1[x, y] <: P[x, y]](f: P1[A, A])(using ProTraverseRep[P1]): A =
    foldEval(f.seq[Eval]).value
end Layer

trait Coalgebra[P[_, _], A]:
  def apply[B](a: A, p: P[A, B]): B

object Layer:
  def unfold[A, P[-_, +_]](ca: Coalgebra[P, A])(init: A)(using ProLeftMap[P]): Layer[P] =
    new:
      def unwrap[X](f: P[Layer[P], X]): X =
        ca(init, f.lmap[A](Layer.unfold(ca)))

  def apply[P[-_, +_], Q[-i, +o] <: P[i, o]](using P: ProTraverseRep[P]): P[Layer[Q], Layer[Q]] =
    P.tabulate(rep => new { def unwrap[A](pla: Q[Layer[Q], A]) = rep(pla) })

trait Bools[-I, +O] derives ProTraverseRep:
  def True: O
  def False: O
  def Not(x: I): O
  def And(x: I, y: I): O
  def Or(x: I, y: I): O

trait Numbers[-I, +O] derives ProTraverseRep:
  def fromInt(x: Int): O
  def plus(x: I, y: I): O
  def multiply(x: I, y: I): O

trait Comparison[-I, +O] derives ProTraverseRep:
  def less(x: I, y: I): O
  def equals(x: I, y: I): O

type &&&[P[-_, +_], Q[-_, +_]] = [a, b] =>> P[a, b] & Q[a, b]

type L[-i, +o] = (Numbers &&& Comparison)[i, o]

val expr: Layer[L] =
  Layer[Comparison, L].equals(
    Layer[Numbers, L].fromInt(4),
    Layer[Numbers, L].plus(
      Layer[Numbers, L].fromInt(1),
      Layer[Numbers, L].fromInt(3)
    )
  )

type L2[-i, +o] = (Numbers &&& Comparison &&& Bools)[i, o]

val expr1: Layer[L2] = expr
