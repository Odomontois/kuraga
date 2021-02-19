package kuraga
package syntax

import scala.quoted.*
import scala.compiletime.*


extension[T <: Tuple](xs: T) inline def zipA(using AZ: AppZipping[T]) : AZ.F[AZ.T] = AZ.zipA(xs)


abstract class AppZipping[TF <: Tuple]:
    type F[_]
    type T <: Tuple
    def zipA(tf: TF): F[T]

object AppZipping:    
    abstract class Aux[TF <: Tuple, G[_], R <: Tuple] extends AppZipping[TF]{type F[a] = G[a]; type T = R}

    given cons[H, G[_]: Applicative, TF <: Tuple, T <: Tuple](using FT: Aux[TF, G, T]): Aux[G[H] *: TF, G, H *: T] with
        def zipA(tf: G[H] *: TF): G[H *: T] = 
            tf.head.map2(FT.zipA(tf.tail))(_ *: _)

    given nil[F[_]](using F: Applicative[F]): Aux[EmptyTuple, F, EmptyTuple] with
        def zipA(tf: EmptyTuple): F[EmptyTuple] = F.pure(EmptyTuple)
        