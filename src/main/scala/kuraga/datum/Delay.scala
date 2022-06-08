package kuraga.datum

import kuraga.datum.Handler.DefaultName

type Delay[-A, +B] = Delay.Delay[A, B]

trait DelayEff[+Y] extends Proeff[Any, Y]:
  self =>
  def delay: Y

  def map[Z](f: Y => Z) = new DelayEff[Z]:
    def delay: Z                     = f(self.delay)
    override def map[Z1](g: Z => Z1) = self.map(f andThen g)
end DelayEff

object Delay:

  type Eff[-A, +B]   = DelayEff[B]
  type Delay[-A, +B] = Handler.Of { val delay: DelayEff[B] }

  given DefaultName[Eff]                                                = DefaultName("delay")
  def handler[P[-_, +_], R](other: => Star[P, R]): Handler[Delay, P, R] =
    Handler.singleVia[Eff](new { def delay = other })

  val intro: Star[Delay, Nothing] =
    Star.Intro([A] => (d: Delay[Nothing, A]) => d.delay.delay)
end Delay
