package playground.talks.prof.hk.prof

import kuraga.{Applicative, Eval}
import playground.talks.prof.ProRep

trait Bools[-I[_], +O[_]]:
  def True: O[Boolean]
  def False: O[Boolean]
  def Not(x: I[Boolean]): O[Boolean]
  def And(x: I[Boolean], y: I[Boolean]): O[Boolean]
  def Or(x: I[Boolean], y: I[Boolean]): O[Boolean]

trait Numbers[-I[_], +O[_]]:
  def fromInt(x: Int): O[Int]
  def plus(x: I[Int], y: I[Int]): O[Int]
  def multiply(x: I[Int], y: I[Int]): O[Int]

trait Comparison[-I[_], +O[_]]:
  def less(x: I[Int], y: I[Int]): O[Boolean]
  def equals(x: I[Int], y: I[Int]): O[Boolean]

trait ProRepHK[P[_[_], _[_]], I[_], A]:
  def apply[O[_]](p: P[I, O]): O[A]

trait ProRepresentableHK[P[_[_], _[_]]]:
  def tabulate[I[_], O[_]](f: [A] => ProRepHK[P, I, A] => O[A]): P[I, O]

trait ProLeftMapHK[P[_[_], _[_]]]:
  def leftMap[I[_], O[_], L[_]](pab: P[I, O])(f: [A] => L[A] => I[A]): P[L, O]

  extension [I[_], O[_]](pab: P[I, O]) def lmap[L[_]](f: [A] => L[A] => I[A]): P[L, O] = leftMap(pab)(f)

trait ProSequenceHK[P[_[_], _[_]]]:
  def sequence[I[_], O[_], F[_]: Applicative](pab: P[I, O]): P[[A] =>> F[I[A]], [A] =>> F[O[A]]]

  extension [I[_], O[_]](pab: P[I, O]) def seq[F[_]: Applicative]: P[[A] =>> F[I[A]], [A] =>> F[O[A]]] = sequence(pab)

trait ProTraverseRepHK[P[_[_], _[_]]] extends ProRepresentableHK[P] with ProLeftMapHK[P] with ProSequenceHK[P]:
  def proTraverse[L[_], I[_], O[_], F[_]: Applicative](
      fromRep: [A] => ProRepHK[P, I, A] => O[A],
      f: [A] => L[A] => I[A]
  ): P[[A] =>> F[L[A]], [A] =>> F[O[A]]]

  override def leftMap[I[_], O[_], L[_]](pab: P[I, O])(f: [A] => L[A] => I[A]): P[L, O] =
    proTraverse[L, I, O, [x] =>> x]([A] => (p: ProRepHK[P, I, A]) => p(pab), f)

  override def sequence[I[_], O[_], F[_]: Applicative](pab: P[I, O]): P[[A] =>> F[I[A]], [A] =>> F[O[A]]] =
    proTraverse[I, I, O, F]([A] => (p: ProRepHK[P, I, A]) => p(pab), [A] => (x: I[A]) => x)

  override def tabulate[I[_], O[_]](f: [A] => ProRepHK[P, I, A] => O[A]): P[I, O] =
    proTraverse[I, I, O, [x] =>> x](f, [A] => (x: I[A]) => x)
end ProTraverseRepHK

trait LayerHK[-P[-_[_], +_[_]], A]:
  def unwrap[O[_]](f: P[LayerHK[P, *], O]): O[A]

  def fold[R[_], P1[i[_], o[_]] <: P[i, o]](f: P1[R, R])(using ProLeftMapHK[P1]): R[A] =
    unwrap(f.lmap[LayerHK[P, *]]([A] => (l: LayerHK[P, A]) => l.fold(f)))

  def foldEval[R[_], P1[i[_], o[_]] <: P[i, o]](f: P1[[A] =>> Eval[R[A]], [A] =>> Eval[R[A]]])(using
      ProLeftMapHK[P1]
  ): Eval[R[A]] =
    unwrap(f.lmap([A] => (l: LayerHK[P, A]) => l.foldEval(f))).defer

  def foldTail[R[_], P1[i[_], o[_]] <: P[i, o]](f: P1[R, R])(using ProTraverseRepHK[P1]): R[A] =
    foldEval(f.seq[Eval]).value
end LayerHK

trait CoalgebraHK[P[-_[_], +_[_]], I[_]]:
  def apply[O[_], A](a: I[A], p: P[I, O]): O[A]

object LayerHK:
  def unfold[I[_], A, P[-_[_], +_[_]]](ca: CoalgebraHK[P, I])(init: I[A])(using
      ProLeftMapHK[P]
  ): LayerHK[P, A] =
    new:
      def unwrap[O[_]](f: P[LayerHK[P, *], O]): O[A] =
        ca(init, f.lmap[I]([B] => (ia: I[B]) => LayerHK.unfold(ca)(ia)))
