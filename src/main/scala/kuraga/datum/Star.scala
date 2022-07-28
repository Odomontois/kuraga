package kuraga.datum

import scala.annotation.targetName
import scala.language.experimental.erasedDefinitions

@targetName("Und")
type &::[A[-_, +_], B[-_, +_]] = [x, y] =>> A[x, y] & B[x, y]

type AnyP[-x, +y] = Any

enum Star[-P[-_, +_], +R]:
  case Intro[-P[-_, +_], +R](f: Repr[P, Star[P, R]])                                      extends Star[P, R]
  case Elim[Q[-_, +_], -P[-_, +_], +R](star: Star[P &:: Q, R], handler: Handler[Q, P, R]) extends Star[P, R]
  case Done(r: R)

  def elimRun[A](handler: Handler[P, AnyP, A]): R | A = Star.foldRun(this)(handler)
end Star

object Star:
  extension [R](star: Star[AnyP, R]) def run: R = foldRun(star)(Handler.Identity)

  extension [P[-_, +_], R](lzStar: => Star[P, R])
    def delay: Star[P, R] = Elim[Delay.Delay, P, R](Delay.intro, Delay.handler(lzStar))

  def foldRun[P[-_, +_], S, R](star: Star[P, S])(handler: Handler[P, AnyP, R]): S | R =
    star match
      case Intro(f)               => foldRun(f(handler.handle))(handler)
      case Elim(starIn, handleIn) => foldRun(starIn)(handler ++ handleIn.andThen(handler))
      case Done(x)                => x
end Star
