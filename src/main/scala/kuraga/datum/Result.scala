package kuraga.datum

import kuraga.datum.Handler.DefaultName
import kuraga.datum.Result.Result

trait ResultEff[-A, +Y] extends Proeff[Any, Y]:
  self =>
  def result(a: A): Y

  override def map[Z](f: Y => Z): ResultEff[A, Z] = new:
    def result(a: A)                 = f(self.result(a))
    override def map[Z1](g: Z => Z1) = self.map(f andThen g)
end ResultEff

object ResultEff:
  extension [P[-_, +_], S, A](star: Star[P &:: Result[A], S]) 
    def flatMap[Q[-_, +_], T](f: A => Star[Q, T]): Star[P &:: Q, S | T] =
      Star.Elim[Result[A], P &:: Q, S | T](star, Result.handler(f))

object Result:
  type Eff[-A]         = [x, y] =>> ResultEff[A, y]
  type Res[-X, +Y, -A] = Handler.Of { val result: ResultEff[A, Y] }
  type Result[-A]      = [X, Y] =>> Res[X, Y, A]

  given [A]: DefaultName[Eff[A]] = DefaultName("result")

  def handler[A, P[-_, +_], R](f: A => Star[P, R]): Handler[Result[A], P, R] =
    Handler.singleVia[Eff[A]](f(_))
end Result
