package chats

import scala.reflect.TypeTest

package nonNull {
  opaque type NonNull = Any

  private inline def c(x: Any): x.type & NonNull = x

  open class NullTest[A] extends TypeTest[A, NonNull] {
    def unapply(x: A): Option[x.type & NonNull] = Option.when(x != null)(c(x))
  }

  object NonNull extends NullTest[Any] {
    inline def apply[x <: Singleton & (Boolean | Byte | Short | Int | Long | Float | Double | Char | String)](
        inline x: x
    ): x & NonNull = {
      compiletime.requireConst(x)
      x.asInstanceOf[x & NonNull]
    }
  }

  given [A]: NullTest[A] with {}
}

export nonNull.NonNull

val check: PartialFunction[String & NonNull, Unit] = { case NonNull(s) =>
  println(s)
}

@main def isItNullYet() = {

  println(1)

  val strs = List[String]("hello", null, "world", null, "!")

  val notNullStrs  = strs.collect { case NonNull(s) => s }
  val notNullStrs2 = strs.collect { case s: (String & NonNull) => s }

  val nums = Vector[Integer](1, 2, null, 4, null, 6)

  val notNullNums  = nums.collect { case NonNull(n) => n }
  val notNullNums2 = nums.collect { case n: (Integer & NonNull) => n }

  println(notNullStrs2: List[String & NonNull])
  println(notNullStrs: List[String & NonNull])
  println(notNullNums: Vector[Integer & NonNull])
  println(notNullNums2: Vector[Integer & NonNull])

  NonNull("1")
//   println(NonNull.apply[2](2): 2 & NonNull)
//   println(NonNull(2): Int & NonNull)
}
