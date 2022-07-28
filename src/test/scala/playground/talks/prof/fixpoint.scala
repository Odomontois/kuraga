package playground.talks.prof.fixpoint

import kuraga.{Eval, Functor, Traverse}
import reflect.Typeable

final case class Fix[+F[+_]](value: F[Fix[F]]):
  def fold[A, F1[x] >: F[x]: Functor](f: F1[A] => A): A = f(value.map(_.fold(f)))

  def foldEval[A, F1[x] >: F[x]: Traverse](f: F1[A] => Eval[A]): Eval[A] =
    value.traverse(_.foldEval(f)).flatMap(f).defer

  def foldTail[A, F1[x] >: F[x]: Traverse](f: F1[A] => A): A =
    foldEval[A, F1](fx => Eval.later(f(fx))).value

object Fix:
  def unfold[F[+_]: Functor, A](init: A)(f: A => F[A]): Fix[F] =
    Fix(f(init).map(Fix.unfold(_)(f)))

  def unfoldEval[F[+_]: Traverse, A](init: A)(f: A => Eval[F[A]]): Eval[Fix[F]] =
    f(init).flatMap(_.traverse(Fix.unfoldEval(_)(f))).map(Fix(_)).defer

  def unfoldTail[F[+_]: Traverse, A](init: A)(f: A => F[A]): Fix[F] =
    unfoldEval(init)(a => Eval.later(f(a))).value
end Fix

enum Bools[+A] derives Traverse:
  case True
  case False
  case Not(x: A)
  case And(x: A, y: A)
  case Or(x: A, y: A)

enum Numbers[+A] derives Traverse:
  case FromInt(x: Int)
  case Plus(x: A, y: A)
  case Multiply(x: A, y: A)

enum Comparison[+A] derives Traverse:
  case Less(x: A, y: A)
  case Equals(x: A, y: A)

type ||[F[+_], G[+_]] = [A] =>> F[A] | G[A]

//format: off
val expr: Fix[Numbers || Comparison] =
  Fix(Comparison.Equals(
      Fix(Numbers.FromInt(4)),
      Fix(Numbers.Plus(
        Fix(Numbers.FromInt(1)),
        Fix(Numbers.FromInt(3))
      ))
    ))
//format: on

val expr1: Fix[Numbers || Comparison || Bools] = expr

def summonTraverse =
  summon[Traverse[Numbers || Comparison || Bools]](using ???)

def handing(x: Int): Int =
  try {
    2 / x + handing(x - 1)
  } catch {
    case _: ArithmeticException => 0
  }

def handle[F[+_], H[+_]](
    fix: Fix[F || H]
)(
    handler: H[Fix[F || H]] => Fix[F]
)(using
    Typeable[H[Fix[F || H]]],
    Typeable[F[Fix[F || H]]],
    Functor[F]
): Fix[F] =
  fix.value match
    case h: H[Fix[F || H]] => handler(h)
    case f: F[Fix[F || H]] => Fix(f.map(handle(_)(handler)))
