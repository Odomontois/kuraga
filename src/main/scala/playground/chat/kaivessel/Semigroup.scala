package playground.chat.kaivessel
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

inline def summonAll[T <: Tuple, TC[_]]: List[TC[Any]] = inline compiletime.erasedValue[T] match 
  case _: Unit => Nil
  case _: (t *: ts) => summonInline[TC[t]].asInstanceOf[TC[Any]] :: summonAll[ts, TC]

trait Semigroup[A]:
  def combine(a: A, b: A): A
  extension (a: A) final def <+> (b: A): A = combine(a, b)

object Semigroup:
  given Semigroup[Int]     = _ + _
  given Semigroup[String]  = _ + _
  given[A]: Semigroup[List[A]] = _ ++ _
  given[A]: Semigroup[Option[A]] = _ orElse _
  

  inline def derived[A] (using m: Mirror.ProductOf[A]): Semigroup[A] =  new:
    val instances = IArray(summonAll[m.MirroredElemTypes, Semigroup] :_*)

    def combine(a: A, b: A) = m fromProduct new:
      val ap = a.asInstanceOf[Product]
      val bp = b.asInstanceOf[Product]
      def productArity = ap.productArity
      def productElement(i: Int) = instances(i).asInstanceOf[Semigroup[Any]].combine(ap.productElement(i), bp.productElement(i))
      def canEqual(that: Any) = true