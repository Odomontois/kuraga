package kuraga.datum

import kuraga.datum.Handler.DefaultName
import kuraga.datum.Result.Result

type GetResult[Q] = Q match
    case Result.Res[s1, s2, a] => a

trait ResultEff[-A, -X, +Y] extends Proeff[Any, Y]:
    self =>
    def result(a: A): Y

    override def map[Z](f: Y => Z): ResultEff[A, X, Z] = new:
        def result(a: A)                 = f(self.result(a))
        override def map[Z1](g: Z => Z1) = self.map(f andThen g)
end ResultEff

object ResultEff:
    extension [P[-_, +_], S, A](star: Star[P &:: Result[A], S])
        def flatMap[Q[-_, +_], T](f: A => Star[Q, T]): Star[P &:: Q, S | T] =
            Star.Elim[Result[A], P &:: Q, S | T](star, Result.handler(f))
end ResultEff

object Result:
    type Eff[-A]                                                                 = [x, y] =>> ResultEff[A, x, y]
    opaque type Res[-X, +Y, -A] <: Handler.Of { val result: ResultEff[A, X, Y] } = Handler.Of {
        val result: ResultEff[A, X, Y]
    }
    type Result[-A]                                                              = [X, Y] =>> Res[X, Y, A]

    given [A]: DefaultName[Eff[A]] = DefaultName("result")

    def handler[A, P[-_, +_], R](f: A => Star[P, R]): Handler[Result[A], P, R] =
        Handler.singleVia[Eff[A]](new { def result(a: A) = f(a) })

    def pure[U](a: U): Star[Result[U], Nothing] = Star.Intro(_ => r => r.result.result(a))
end Result
