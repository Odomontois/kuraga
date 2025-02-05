import scala.deriving.Mirror
import scala.compiletime.summonInline
case class Address(country: String | Null, state: String | Null, city: String | Null)

val testCases = List(
  ((null, null, null), (null, null, null), true),                          // true
  (("USA", null, "New York"), ("USA", "NY", null), true),                  // true
  (("USA", null, null), ("USA", null, null), true),                        // true
  (("Germany", "Berlin", "Berlin"), ("Germany", "Berlin", null), true),    // true
  (("France", "Paris", "Lyon"), ("France", "Paris", "Lyon"), true),        // true
  (("Canada", null, "Toronto"), ("Canada", "Ontario", "Toronto"), true),   // true
  ((null, "California", null), (null, "California", "Los Angeles"), true), // true
  (("Italy", "Rome", null), ("Italy", null, "Rome"), true),                // true
  (("Japan", "Tokyo", "Shibuya"), ("Japan", "Kyoto", "Osaka"), false),     // false
  (("Poland", null, "New York"), ("USA", "NY", null), false),              // false
)

testCases.forall { (a, b, res) =>
    val m     = summon[Mirror.ProductOf[Address]]
    val addrA = m.fromTuple(a)
    val addrB = m.fromTuple(b)
    compareProduct(addrA, addrB) == res
}

inline def compareProduct[A <: Product](a: A, b: A)(using
    m: Mirror.ProductOf[A]
)(using mt: Mirror.ProductOf[m.MirroredElemTypes]): Boolean =
    compareTuples(mt.fromProduct(a), mt.fromProduct(b))

inline def compareTuples[T <: Tuple](a: T, b: T): Boolean = {
    inline a match {
        case _: ((t | Null) *: ts) =>
            val convert = summonInline[T <:< (t | Null) *: ts]
            val at      = convert(a)
            val bt      = convert(b)
            (at.head == bt.head || at.head == null || bt.head == null) && compareTuples[ts](at.tail, bt.tail)
        case _: EmptyTuple         => true
    }
}
2 + 3
