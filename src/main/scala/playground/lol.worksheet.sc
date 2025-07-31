type Tuple = [A, B] =>> [R] => (A => B => R) => R
val fst   = [A, B] => (t: Tuple[A, B]) => t(x => y => x)
val snd   = [A, B] => (t: Tuple[A, B]) => t(x => y => y)
val tuple = [A, B] => (x: A) => (y: B) => [R] => (f: A => B => R) => f(x)(y)

type Sum = [A, B] =>> [R] => (A => R) => (B => R) => R
val inl: [A, B] => A => Sum[A, B] = [A, B] => (x: A) => [R] => (f: A => R) => (g: B => R) => f(x)
val inr: [A, B] => B => Sum[A, B] = [A, B] => (y: B) => [R] => (f: A => R) => (g: B => R) => g(y)

type Un = [A] => (x: A) => A
val unit = [A] => (x: A) => x

type Opt = [A] =>> Sum[Un, A]
val opt = [A, R] => (opt: Opt[A]) => (n: R) => (s: A => R) => opt(_ => n)(s)

type Nat = [A] => (A) => (A => A) => A

val zero: Nat               = [A] => (z: A) => (s: A => A) => z
val succ: Nat => Nat        = x => [A] => (z: A) => (s: A => A) => s(x(z)(s))
val plus: Nat => Nat => Nat = x => y => x[Nat](y)(succ)
val prod: Nat => Nat => Nat = x => y => x[Nat](zero)(plus(y))

val optPlus: Opt[Nat] => Nat = x => opt[Nat, Nat](x)(succ(zero))(succ)
val pred: Nat => Nat         = x =>
    fst[Nat, Nat](x[Tuple[Nat, Nat]](tuple(zero)(zero))(t => tuple(snd[Nat, Nat](t))(succ(snd[Nat, Nat](t)))))
val predO: Nat => Opt[Nat]   = x =>
    fst[Opt[Nat], Nat](
      x[Tuple[Opt[Nat], Nat]](tuple(inl[Un, Nat](unit))(zero))(t =>
          tuple(inr[Un, Nat](snd[Opt[Nat], Nat](t)))(succ(snd[Opt[Nat], Nat](t)))
      )
    )

def toInt(x: Nat) = x[Int](0)(_ + 1)

def toOption[A](xo: Opt[A]): Option[A] = opt(xo)(None)(Some(_))

val two  = succ(succ(zero))
val four = plus(two)(two)
val five = succ(four)
val ten  = prod(five)(two)

toInt(ten)
toInt(pred(ten))

toOption(predO(ten)).map(toInt)
toOption(predO(zero)).map(toInt)

1 + 2
