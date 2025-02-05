import scala.deriving.Mirror
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

def compareProduct[A <: Product](a: A, b: A): Boolean = {
    a.productIterator.zip(b.productIterator).forall { (aVal, bVal) =>
        aVal == bVal || aVal == null || bVal == null
    }
}
