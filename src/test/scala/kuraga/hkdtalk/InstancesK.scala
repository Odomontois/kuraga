package kuraga
package hkdtalk
import kuraga.hkd._
import cats.Show

given Names[PersonOf] = names[PersonOf]

given PersonOf[Show] = provision[PersonOf[Show]]


given ApplicativeK[PersonOf] with TraverseK[PersonOf] with
  extension [F[_], G[_]] (p: PersonOf[F]) 
    override def mapK (f: [A] => F[A] => G[A]): PersonOf[G] = 
        PersonOf(
            f(p.firstName),
            f(p.lastName),
            f(p.age),
            f(p.tags),
        )

  extension [F[_], G[_], H[_]] (left: PersonOf[F])
    def map2K(right: PersonOf[G])
    (f: [A] => (F[A], G[A]) => H[A]): PersonOf[H] = 
      PersonOf(
        f(left.firstName, right.firstName),
        f(left.lastName, right.lastName),
        f(left.age, right.age),
        f(left.tags, right.tags)
      )

  def pureK[F[_]](gen: [A] => () => F[A]): PersonOf[F] = 
    PersonOf(
      gen(),
      gen(),
      gen(),
      gen(),
    )

  extension[F[_], G[+_], H[_]](uf: PersonOf[F])
    def traverseK(f: [A] => F[A] => G[H[A]])(
      using Applicative[G]): G[PersonOf[H]] = 
        f(uf.firstName).map4(
          f(uf.lastName),
          f(uf.age),
          f(uf.tags)
        )(PersonOf(_, _, _, _))

