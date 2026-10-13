import scala.reflect.Typeable

object notNull {
  opaque type NotNull = Any

  given NotNull: Typeable[NotNull] with {
    def unapply(x: Any): Option[x.type & NotNull] =
      if x != null then Some(x.asInstanceOf[x.type & NotNull]) else None
  }
}

import notNull.NotNull

val strs = List[String]("hello", null, "world", null, "!")

strs.collect { case s: NotNull => s }

val notNullStrs = strs.collect { case NotNull(s) => s }

val nums = Vector[Integer](1, 2, null, 4, null, 6)

val notNullNums = nums.collect { case NotNull(n) => n }

notNullStrs: List[String & NotNull]
notNullNums: Vector[Integer & NotNull]

case class NonNullString private (value: String) //or opaque type

object NonNullString {
  inline def make(v: String): NonNullString =
    if v == null
    then compiletime.error("no null please")
    else NonNullString(v)
}

val s = NonNullString.make("foo")
val x = "foo"
// NonNullString.make("foo" * 2)
println(s)

// def wontCompile = NonNullString.make(null)
