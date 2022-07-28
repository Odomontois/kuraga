package playground.talks.prof.repr

import kuraga.Functor

trait Repr[-F[_]]:
  def apply[B](fb: F[B]): B

trait Representable[F[_]] extends Functor[F]:
  def tabulate[A](f: Repr[F] => A): F[A]

  extension [A](fa: F[A]) override def map[B](f: A => B) = tabulate(repr => f(repr(fa)))

def index[F[_], A](f: F[A]): Repr[F] => A = _(f)

trait FromUser[R]:
  def anonymous: R
  def authorized(name: String, email: String, friends: Vector[R]): R
  def admin(key: Seq[Byte]): R

enum User:
  case Anonymous
  case Authorized(name: String, email: String, friends: Vector[User])
  case Admin(key: Seq[Byte])

def toRepr(user: User): Repr[FromUser] = user match
  case User.Anonymous                        =>
    new { def apply[B](f: FromUser[B]) = f.anonymous }
  case User.Admin(key)                       =>
    new { def apply[B](f: FromUser[B]) = f.admin(key) }
  case User.Authorized(name, email, friends) =>
    new { def apply[B](f: FromUser[B]) = f.authorized(name, email, friends.map(toRepr(_)(f))) }

def fromRepr(repr: Repr[FromUser]): User = repr(new FromUser[User] {
  def anonymous                                                            =
    User.Anonymous
  def admin(key: Seq[Byte])                                                =
    User.Admin(key)
  def authorized(name: String, email: String, friends: Vector[User]): User =
    User.Authorized(name, email, friends)
})

enum Bool:
  case False, True

trait FromBool[A]:
  def False: A
  def True: A

enum Nat:
  case Zero
  case Suc(prev: Nat)

trait FromNat[A]:
  def Zero: A
  def Suc(prev: A): A

enum List[A]:
  case Nil()
  case Cons(head: A, tail: List[A])

trait FromList[A, B]:
  def Nil: B
  def Cons(head: A, tail: B): B

trait OfNotFound[R]:
  def notFound(id: String): R

case class NotFound(id: String)

trait OfAlreadyExists[R]:
  def alreadyExists(id: String): R

case class AlreadyExists(id: String)

trait `OfNotFound & OfAlreadyExits`[R]:
  def notFound(id: String): R
  def alreadyExists(id: String): R

enum `NotFound | AlreadyExists`:
  case NotFound(id: String)
  case AlreadyExists(id: String)

trait OfBools[A]:
  def True: A
  def False: A
  def Not(x: A): A
  def And(x: A, y: A): A
  def Or(x: A, y: A): A

enum Bools:
  case True
  case False
  case Not(x: Bools)
  case And(x: Bools, y: Bools)
  case Or(x: Bool, y: Bools)

trait OfNumbers[A]:
  def fromInt(x: Int): A
  def plus(x: A, y: A): A
  def multiply(x: A, y: A): A

enum Numbers:
  case FromInt(x: Int)
  case Plus(x: Numbers, y: Numbers)
  case Multiply(x: Numbers, y: Numbers)

trait OfComparison[A]:
  def less(x: A, y: A): A
  def equals(x: A, y: A): A

enum Comparison:
  case Less(x: Comparison, y: Comparison)
  case Equals(x: Comparison, y: Comparison)

type &&[F[_], G[_]] = [a] =>> F[a] & G[a]

val expr: Repr[OfNumbers && OfComparison] = new {
  def apply[A](L: OfNumbers[A] & OfComparison[A]) =
    L.equals(
      L.fromInt(4),
      L.plus(
        L.fromInt(1),
        L.fromInt(3)
      )
    )
}

val expr1: Repr[OfNumbers && OfComparison && OfBools] = expr

trait `OfBools & OfNumbers & OfComparison`[A]:
  def True: A
  def False: A
  def Not(x: A): A
  def And(x: A, y: A): A
  def Or(x: A, y: A): A

  def fromInt(x: Int): A
  def plus(x: Int, y: Int): A
  def multiply(x: Int, y: Int): A

  def less(x: A, y: A): A
  def equals(x: A, y: A): A
end `OfBools & OfNumbers & OfComparison`

enum Expr:
  case True
  case False
  case Not(x: Expr)
  case And(x: Expr, y: Expr)
  case Or(x: Expr, y: Expr)
  case FromInt(x: Int)
  case Plus(x: Expr, y: Expr)
  case Multiply(x: Expr, y: Expr)
  case Less(x: Expr, y: Expr)
  case Equals(x: Expr, y: Expr)
end Expr

def prev(n: Repr[FromNat]): Repr[FromNat] =
  n(new FromNat[(Repr[FromNat], Repr[FromNat])] {
    def Zero: (Repr[FromNat], Repr[FromNat]) =
      (FromNat.Zero, FromNat.Zero)

    def Suc(prev: (Repr[FromNat], Repr[FromNat])) =
      (prev._2, FromNat.Suc(prev._1))
  })._1

object FromNat extends FromNat[Repr[FromNat]]:
  def Zero: Repr[FromNat]                     =
    new { def apply[A](f: FromNat[A]) = f.Zero }
  def Suc(prev: Repr[FromNat]): Repr[FromNat] =
    new { def apply[A](f: FromNat[A]) = f.Suc(prev(f)) }
