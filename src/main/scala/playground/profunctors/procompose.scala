package playground.profunctors
import cats.arrow.Profunctor
import cats.syntax.profunctor._
import scala.language.implicitConversions

enum Procompose[P[_, _], Q[_, _], A, B]{
  case Impl[P[_, _], Q[_, _], A, M, B](pam: P[A, M], qmb: Q[M, B]) extends Procompose[P, Q, A, B]
}

object Procompose:
  given [P[_, _]: Profunctor, Q[_, _]: Profunctor] :  Profunctor[P |::| Q] with
    def dimap[A, B, C, D](fab: Procompose[P, Q, A, B])(f: C => A)(g: B => D): Procompose[P, Q, C, D] = 
     fab match { case Impl(pam, qmb) => Impl(pam.lmap(f), qmb.rmap(g))  }

type |::| [P[_, _], Q[_, _]] = [A, B] =>> Procompose[P, Q, A, B]

trait ~~>[P[_, _], Q[_, _]]{ pt => 
  def apply[A, B](pab: P[A, B]): Q[A, B]

  def compose[T[_, _]](t: T ~~> P): T ~~> Q = new (T ~~> Q){
    def apply[A, B](pab: T[A, B]) = pt(t(pab))
  }  

  def hcomp[S[_, _], T[_, _]](t: S ~~> T): ((P |::| S) ~~> (Q |::| T)) =
    new ((P |::| S) ~~> (Q |::| T)) {
      def apply[A, B](pab: Procompose[P, S, A, B]): Procompose[Q, T, A, B] =
        pab match { case Procompose.Impl(pam, qmb) => Procompose.Impl(pt(pam), t(qmb)) }        
    }
}
