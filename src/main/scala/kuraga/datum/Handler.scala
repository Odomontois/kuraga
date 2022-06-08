package kuraga.datum

class Handler[+I[-_, +_], -O[-_, +_], +R](val values: Map[String, Proeff.Handle[O, R]]) extends Selectable:
  def handle[U[-_, +_], S]: I[Star[U, S], Star[U &:: O, S | R]]                             = this.asInstanceOf[I[Star[U, S], Star[U &:: O, S | R]]]
  def selectDynamic(name: String): Any                                                      = values(name)
  def ++[V[-_, +_], L[-_, +_], S](that: Handler[V, L, S]): Handler[I &:: V, O &:: L, R | S] =
    Handler(values ++ that.values)
  def andThen[T[-_, +_], S](that: Handler[O, T, S]): Handler[I, T, R | S]                   =
    def mapstar(star: Star[O, R]) = Star.Elim[O, T, R | S](star, that)
    Handler(values.view.mapValues(_.map(mapstar)).toMap)

end Handler

object Handler:
  object Identity extends Handler[AnyP, AnyP, Nothing](Map.empty)

  def single[I[-_, +_], O[-_, +_], R](
      name: String,
      handler: Proeff.Handle[O, R]
  ): Handler[I, O, R] = Handler[I, O, R](Map(name -> handler))

  type Of = Handler[AnyP, Nothing, Any]
end Handler

trait Proeff[-X, +Y]:
  type Me[-x, +y] <: Proeff[x, y]
  type This >: this.type <: Me[X, Y]

  def map[Z](f: Y => Z): Me[X, Z]
end Proeff

object Proeff:
  type Handle[-O[-_, +_], +R] = Proeff[Star[Nothing, Any], Star[O, R]]
end Proeff
