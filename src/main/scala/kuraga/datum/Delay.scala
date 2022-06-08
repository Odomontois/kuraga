package kuraga.datum

type Delay[-A, +B] = Delay.Delay[A, B]

trait DelayEff[+Y] extends Proeff[Any, Y]:
  self =>
  def delay: Y

  type Me[-x, +y] = DelayEff[y]
  type This >: this.type <: DelayEff[Y]

  def map[Z](f: Y => Z) = new DelayEff[Z]:
    def delay: Z                     = f(self.delay)
    override def map[Z1](g: Z => Z1) = self.map(f andThen g)
end DelayEff


object Delay:

  type Delay[-A, +B] = Handler.Of { val delay: DelayEff[B] }
  def handler[P[-_, +_], R](other: => Star[P, R]): Handler[Delay, P, R] =
    Handler.single(
      "delay",
      new DelayEff[Star[P, R]]:
        def delay = other
    )

  val intro: Star[Delay, Nothing] =
    Star.Intro([A] => (d: Delay[Nothing, A]) => d.delay.delay)
end Delay