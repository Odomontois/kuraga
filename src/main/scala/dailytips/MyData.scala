package dailytips

import kuraga.Monoid
import cats.*
import cats.syntax.all.given
import scala.deriving.Mirror
import scala.compiletime.summonAll

trait Givens:
  inline given [X <: Product](using m: Mirror.ProductOf[X]): X =
    m.fromProduct(summonAll[m.MirroredElemTypes])

  def useAll[X <: Product, A](using x: X, m: Mirror.ProductOf[X])(f: Givens.UseAll[m.MirroredElemTypes, A]): A =
    val elems = Tuple.fromProductTyped(x)

    ???
  end useAll

  private def useIter[T <: Tuple, A](f: Givens.UseAll[T, A])(t: T): A =
    t match
      case _: EmptyTuple  => ???
      case _: (h *: rest) => ???
end Givens

object Givens:
  type UseAll[T <: Tuple, A] = T match
    case EmptyTuple => A
    case h *: rest  => h ?=> UseAll[rest, A]
end Givens

case class MyData[A](show: Show[A], order: Order[A])

object MyData extends Givens

//def showDifferent[A](xs: List[A])(using mydata: MyData[A]): String =
//  mydata match
//    case MyData(given Show[A], given Order[A]) =>
