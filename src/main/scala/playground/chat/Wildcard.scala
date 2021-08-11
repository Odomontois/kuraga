package playground.chat

// object Wildcard:
//   opaque type Wildcard[k <: AnyKind, F[_ <: k]] = F[Nothing]

//   def apply[k <: AnyKind, F[_ <: k], A <: k](fa: F[A]): Wildcard[k, F] = fa.asInstanceOf[F[Any]]

// type Wildcard[k <: AnyKind, F[_ <: k]] = Wildcard.Wildcard[k, F]

// extension [k <: AnyKind, F[ _ <: k], R] (w: Wildcard[k, F]) def wildFold (f: [a <: k] => F[a] => R): R =
//     f[Any](w.asInstanceOf[F[Any]])

// case class X[R[_]](val r: R[Int], s: String)

// val a: Wildcard[[_] =>> Any, X] = Wildcard[[_] =>> Any, X, List](X(List(1), "lol"))

@main def run() =
  println("Hello")

object Fix:
  opaque type T[+F[+_]] = ApplyFix.T[F]

  def apply[F[+_]](f: F[Fix[F]]): T[F] = ApplyFix(f)

  extension [F[+_]](fix: T[F]) def unwrap: F[Fix[F]] = ApplyFix.unwrap(fix)

  object ApplyFix:
    opaque type T[+F[+_]] = F[Fix[F]]

    def apply[F[+_]](f: F[Fix[F]]): T[F] = f

    def unwrap[F[+_]](v: T[F]): F[Fix[F]] = v

type Fix[+F[+_]] = Fix.T[F]
