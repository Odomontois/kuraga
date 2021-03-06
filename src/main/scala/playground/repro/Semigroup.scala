// package playground.repro

// import scala.compiletime.{erasedValue, summonInline}
// import scala.deriving.Mirror

// inline def summonAll[T <: Tuple, TC[Any]]: List[TC[Any]] = inline erasedValue[T] match
//   case _: Unit => Nil
//   case _: (t *: ts) => summonInline[TC[t]].asInstanceOf[TC[Any]] :: summonAll[ts, TC]

// trait Semigroup[A]:
//   def combine(a: A, b: A): A
//   final def (a: A) <+> (b: A): A = combine(a, b)

// object Semigroup:
//   given Semigroup[Int]     = _ + _
//   given Semigroup[String]  = _ + _
//   given[A] as Semigroup[List[A]] = _ ++ _
//   given[A] as Semigroup[Option[A]] = _ orElse _

//   inline def derived[A] (using m: Mirror.Of[A]): Semigroup[A] = inline m match
//     case p: Mirror.ProductOf[A] => productOf(p)
//     case _ => compiletime.error("can derive only products")

//   private inline def productOf[A](m: Mirror.ProductOf[A]): Semigroup[A] = new:
//     val instances = IArray(summonAll[m.MirroredElemTypes, Semigroup]:_*)

//     def combine(a: A, b: A) = m fromProduct new:
//       val ap = a.asInstanceOf[Product]
//       val bp = b.asInstanceOf[Product]
//       def productArity = ap.productArity
//       def productElement(i: Int) = instances(i).combine(ap.productElement(i), bp.productElement(i))
//       def canEqual(that: Any) = true

// final case class ExampleValue[c[_[_]]](get: c[Option])

// object ExampleValue:
//     given [c[_[_]]] :  Semigroup[ExampleValue[c]] = Semigroup.derived
