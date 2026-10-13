import scala.reflect.Typeable
import scala.compiletime.constValue

object notNull {
  opaque type NotNull = Any

  given NotNull: Typeable[NotNull] with {
    inline def apply[x <: Singleton & (Boolean | Byte | Short | Int | Long | Float | Double | Char | String)](
        inline x: x
    ): x & NotNull = {
      compiletime.requireConst(x)
      x
    }
    def unapply(x: Any): Option[x.type & NotNull] = Option.when(x != null)(x)
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

NotNull("1")
val x: 2 = NotNull(2)

// def wontCompile = NonNullString.make(null)
