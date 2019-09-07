package kuraga.free
import cats._
import cats.arrow.FunctionK
import cats.evidence.As

trait Carrier{
  type T
}

type ||[F[_], G[_]] = [A] =>> F[A] | G[A] 
type Void = [x] =>> Nothing

def funK[F[_], G[_]](f: (c: Carrier) => F[c.T] => G[c.T]): F ~> G = 
  new FunctionK[F, G]{
    def apply[A](fa: F[A]) = f(new Carrier{type T = A})(fa)
  }

class widen[G[+_], X](fa: G[X]){
  def to[H >: G <: [+A] =>> Any, Y >: X]: H[Y] = fa  
}