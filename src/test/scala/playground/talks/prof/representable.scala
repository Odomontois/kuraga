package playground.talks.prof.representable

import kuraga.Functor

trait Representable[F[_]] extends Functor[F]:
  type Representation

  def index[A](fa: F[A]): Representation => A

  def tabulate[A](f: Representation => A): F[A]

  extension [A](fa: F[A]) override def map[B](f: A => B): F[B] = tabulate(r => f(index(fa)(r)))

given [R]: Representable[[a] =>> R => a] with
  type Representation = R

  def index[A](fa: R => A): Representation => A = fa

  def tabulate[A](f: R => A): R => A = f

given [R]: Representable[[a] =>> (a, a)] with
  type Representation = Boolean

  def index[A](fa: (A, A)): Boolean => A = if _ then fa._1 else fa._2

  def tabulate[A](f: Boolean => A): (A, A) = (f(true), f(false))

enum User:
  case Anonymous
  case Authorized(name: String, email: String)
  case Admin(key: Seq[Byte])

trait FromUser[R]:
  def anonymous: R
  def authorized(name: String, email: String): R
  def admin(key: Seq[Byte]): R

given Representable[FromUser] with
  type Representation = User

  def index[A](fa: FromUser[A]): User => A = _ match
    case User.Anonymous               => fa.anonymous
    case User.Authorized(name, email) => fa.authorized(name, email)
    case User.Admin(key)              => fa.admin(key)

  def tabulate[A](f: User => A): FromUser[A] = new:
    def anonymous                               = f(User.Anonymous)
    def authorized(name: String, email: String) = f(User.Authorized(name, email))
    def admin(key: Seq[Byte])                   = f(User.Admin(key))
end given


