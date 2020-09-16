package playground.day
import kuraga.{~>}
import cats.free.Cofree
import cats.syntax.apply._
import cats.syntax.coflatMap._
import cats.syntax.comonad._
import cats.syntax.functor._
import cats.syntax.semigroupal._
import cats.{~> => _, _}

type DayF[F[_], G[_]] = [A] =>> Day[F, G, A]
type CofreeF[F[_]] = [A] =>> Cofree[F, A]

trait Day[F[_], G[_], A]:
  type X
  type Y
  def fx: F[X]
  def gy: G[Y]
  def run(x: X, y: Y): Eval[A]
  def comb: (X, Y) => Eval[A] = run
  def mapKFirst[H[_]](fk: F ~> H): Day[H, G, A]
  def mapKSecond[H[_]](fk: G ~> H): Day[F, H, A]
  def map[B](f: A => B): Day[F, G, B]                 = Day[F, G, X, Y, B](fx, gy)((x, y) => comb(x, y).map(f))
  def fold[H[_]: Apply](fk: F ~> H, gk: G ~> H): H[A] = fk(fx).map2(gk(gy))(comb).map(_.value)
  def swap: Day[G, F, A]                              = Day(gy, fx)((y, x) => comb(x, y))

  def projectL[B](implicit F: Functor[F], G: Comonad[G]): F[A] = fx.map(run(_, gy.extract).value)
  def projectR[B](implicit F: Comonad[F], G: Functor[G]): G[A] = gy.map(run(fx.extract, _).value)


object Day extends DayImplicits:
    def apply[F[_], G[_], X, Y, A](fx: F[X], gy: G[Y])(comb: (X, Y) => Eval[A]): Day[F, G, A] = Impl(fx, gy, comb)

    def combine[F[_], G[_], X, Y, A](fx: F[X], gy: G[Y])(comb: (X, Y) => A): Day[F, G, A] =
        Impl(fx, gy, (x, y) => Eval.later(comb(x, y)))

    def left[F[_], G[_], A](fa: F[A]) (using G: InvariantMonoidal[G]): Day[F, G, A] = combine(fa, G.unit)((a, _) => a)

    def right[F[_], G[_], A](ga: G[A]) (using F: InvariantMonoidal[F]): Day[F, G, A] = Day.combine(F.unit, ga)((_, a) => a)

    extension [F[_], G[_], A](dfg: Day[CofreeF[F], CofreeF[G], A]):
      def zip: Cofree[DayF[F, G], A] = Cofree(
        dfg.comb(dfg.fx.head, dfg.gy.head).value,
        (dfg.fx.tail, dfg.gy.tail).mapN((fx, gy) => Day(fx, gy)((x, y) => Eval.later(Day(x, y)(dfg.comb).zip)))
      )    

    def zipK[F[_], G[_]]: DayF[CofreeF[F], CofreeF[G]] ~> CofreeF[DayF[F, G]] =
        [X] => (day: Day[CofreeF[F], CofreeF[G], X]) => day.zip



    private class Impl[F[_], G[_], XX, YY, A](
        val fx: F[XX], 
        val gy: G[YY], 
        override val comb: (XX, YY) => Eval[A]) extends Day[F, G, A]:
        
        type X = XX
        type Y = YY

        override def run(x: XX, y: YY): Eval[A] = comb(x, y)
        override def mapKFirst[H[_]](fk: F ~> H): Day[H, G, A]  = Day(fk(fx), gy)(comb)
        override def mapKSecond[H[_]](fk: G ~> H): Day[F, H, A] = Day(fx, fk(gy))(comb)



  
   

trait DayImplicits extends DayImplicits1:
    given [F[_]: Comonad, G[_]: Comonad, A] as Comonad[DayF[F, G]] = new DayComonad

trait DayImplicits1 extends DayImplicits2:
    given [F[_]: CoflatMap, G[_]: CoflatMap, A] as CoflatMap[DayF[F, G]] = new DayCoflatMap

trait DayImplicits2 extends DayImplicits3:
    given[F[_]: InvariantMonoidal, G[_]: InvariantMonoidal, A] as Applicative[DayF[F, G]] = new DayApplicative 

trait DayImplicits3 extends DayImplicits4:
    given[F[_]: Semigroupal, G[_]: Semigroupal, A] as Apply[DayF[F, G]] = new DayApply

trait DayImplicits4:
    given [F[_], G[_]] as Functor[DayF[F, G]] = new DayFunctor


class DayFunctor[F[_], G[_]] extends Functor[DayF[F, G]] :
    override def map[A, B](fa: Day[F, G, A])(f: A => B): Day[F, G, B] = fa.map(f)
  

class DayApply[F[_]: Semigroupal, G[_]: Semigroupal] extends DayFunctor[F, G] with Apply[DayF[F, G]] :
    override def ap[A, B](ff: Day[F, G, A => B])(fa: Day[F, G, A]): Day[F, G, B] = map2(ff, fa)(_(_))
    override def map2[A, B, Z](fa: Day[F, G, A], fb: Day[F, G, B])(f: (A, B) => Z): Day[F, G, Z] =
        Day(fa.fx.product(fb.fx), fa.gy.product(fb.gy)) {
          case ((ax, bx), (ay, by)) => fa.run(ax, ay).map2(fb.run(bx, by))(f)
        }
  

class DayApplicative[F[_]: InvariantMonoidal, G[_]: InvariantMonoidal] extends DayApply[F, G] with Applicative[DayF[F, G]] :
    override val unit: Day[F, G, Unit] =
        Day(summon[InvariantMonoidal[F]].unit, summon[InvariantMonoidal[G]].unit)((_, _) => Eval.now(()))
    override def pure[A](x: A): Day[F, G, A] = map(unit)(_ => x)
  

class DayCoflatMap[F[_]: CoflatMap, G[_]: CoflatMap] extends DayFunctor[F, G] with CoflatMap[DayF[F, G]] :
    override def coflatMap[A, B](fa: Day[F, G, A])(f: Day[F, G, A] => B): Day[F, G, B] =
        Day(fa.fx.coflatten, fa.gy.coflatten)((fx, gy) => Eval.later(f(Day(fx, gy)(fa.comb))))
  

class DayComonad[F[_]: Comonad, G[_]: Comonad] extends DayCoflatMap[F, G] with Comonad[DayF[F, G]] :
    override def extract[A](x: Day[F, G, A]): A = x.run(x.fx.extract, x.gy.extract).value
  
  
