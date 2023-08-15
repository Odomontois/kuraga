enum LanguageF[+F[_], A]:
    case Plus[+F[_]](x: F[Int], y: F[Int])        extends LanguageF[F, Int]
    case Lt[+F[_]](x: F[Int], y: F[Int])          extends LanguageF[F, Boolean]
    case And[+F[_]](x: F[Boolean], y: F[Boolean]) extends LanguageF[F, Boolean]
    case LitInt(x: Int)                           extends LanguageF[Nothing, Int]
    case Not[+F[_]](x: F[Boolean])                extends LanguageF[F, Boolean]

// case class Fix[+T[+rec[_], res], A](value: T[Fix[T, *], A])

// type Expr[A] = Fix[[f[_], r] =>> Language[f, r] | Language[f, r] | Language[f, r], A]

enum LanguageQ[A, B]:
    case Plus           extends LanguageQ[(Int, Int), Tuple1[Int]]
    case Lt             extends LanguageQ[(Boolean, Boolean), Tuple1[Boolean]]
    case And            extends LanguageQ[(Int, Int), Tuple1[Boolean]]
    case LitInt(x: Int) extends LanguageQ[EmptyTuple, Tuple1[Int]]
    case Not            extends LanguageQ[Tuple1[Boolean], Tuple1[Boolean]]
