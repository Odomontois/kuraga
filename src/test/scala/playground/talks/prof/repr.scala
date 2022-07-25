package playground.talks.prof.repr

import kuraga.Functor

trait Repr[F[_]]:
  def apply[B](fb: F[B]): B

trait Representable[F[_]] extends Functor[F]:
  def tabulate[A](f: Repr[F] => A): F[A]

  extension [A](fa: F[A]) override def map[B](f: A => B) = tabulate(repr => f(repr(fa)))

def index[F[_], A](f: F[A]): Repr[F] => A = _(f)

trait FromUser[R]:
  def anonymous: R
  def authorized(name: String, email: String, friends: List[R]): R
  def admin(key: Seq[Byte]): R

enum User:
  case Anonymous
  case Authorized(name: String, email: String, friends: List[User])
  case Admin(key: Seq[Byte])

def toRepr(user: User): Repr[FromUser] = user match
  case User.Anonymous                        =>
    new { def apply[B](f: FromUser[B]) = f.anonymous }
  case User.Admin(key)                       =>
    new { def apply[B](f: FromUser[B]) = f.admin(key) }
  case User.Authorized(name, email, friends) =>
    new { def apply[B](f: FromUser[B]) = f.authorized(name, email, friends.map(toRepr(_)(f))) }

def fromRepr(repr: Repr[FromUser]): User = repr(new FromUser[User] {
  def anonymous                                                          =
    User.Anonymous
  def admin(key: Seq[Byte])                                              =
    User.Admin(key)
  def authorized(name: String, email: String, friends: List[User]): User =
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
  case Nil
  case Cons(head: A, tail: List[A])

trait FromList[A, B]:
  def Nil: B
  def Cons(head: A, tail: B): B
