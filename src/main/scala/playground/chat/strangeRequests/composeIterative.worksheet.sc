import cats._
import cats.syntax.all._

def descendants[F[_]: Monad : MonoidK, A](children: A => F[A])(start: A): F[A] = 
	start.pure[F].combineK(children(start).flatMap(descendants(children)))



