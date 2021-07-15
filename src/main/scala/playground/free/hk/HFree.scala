package free.hk

type Go[-F[+_], +U[+f[+_], +_]] = [A] => F[A] => HFree[U, A]

enum HFree[+U[+F[+_], +_], +E]:
  case Yield[+E](e: E) extends HFree[Nothing, E]
  case Bind[+U[+F[+_], +_], A, +E](hf: HFree[U, A], cont: A => HFree[U, E]) extends HFree[U, E]
  case Cover[+U[+F[+_], +A], F[+_], +E](uf: U[F, E], hcont: Go[F, U]) extends HFree[U, E]

// case class Bar[F[_], +G[_]](f: [B] => F[B] => G[B])
// case class Foo[-F[_], G[_]](f: [B] => F[B] => G[B])
// case class Baz[F[_], G[_]](f: [B] => F[B] => G[B])

class Lol[A]

object Lol:
  transparent inline def derived[A]: Lol[A] =
    val kek: Kek[A, String] = new {}
    kek

class Kek[A, B] extends Lol[A]

case class Cheburek() derives Lol

object Check extends App:
  summon[Lol[Cheburek]]
// summon[Kek[Cheburek, String]]
