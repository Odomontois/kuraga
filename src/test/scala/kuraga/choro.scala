package kuraga

import java.util.UUID

// trait Base:
//     def print(): Unit

// class BaseImpl(val x: Int) extends Base:
//     def print() = println(x)

// class Derived(x: Base) extends Base:
//     export x.*

// Derived(BaseImpl(100)).print()


implicit class LongOps(private val x: Long)/*  extends AnyVal */{
    def bytes: Array[Byte] = 
        Array((x >> 24).toByte, 
          (x >> 16).toByte, 
          (x >> 8).toByte, 
          (x & (1 << 8)).toByte)
}

extension (x: Long) def bytes: Array[Byte]= 
    Array((x >> 24).toByte, 
          (x >> 16).toByte, 
          (x >> 8).toByte, 
          (x % 256).toByte)

// trait ToBytes[A]:
//     extension(a: A) def bytes: Array[Byte]

// trait ToBytes[A]{
//     def bytes(a: A): Array[Byte]
// }

// implicit class ToBytesOps[A](private val a: A) /* extends AnyVal */{
//     def bytes(implicit tb: ToBytes[A]): Array[Byte] = tb.bytes(a)
// }

// -256L.bytes.toList

// def foo = 
//     val x = 1
//     val y = 2
//     x + y

// val one = "one"
// one match
//     case "one" => 1
//     case "two" => 2

// def bar = 
//     var x = 0
//     var s = 0
//     while x < 10 do
//         s += x
//         x += 1
//     s

// bar

val xs = List.range(1, 5)

def foo(x: Int) = 
    List(x, -x)
end foo

val X = 2
object Momo extends App:

  for x <- xs if x > 0
      y <- foo(x)
  yield x * y
  end for

  for x <- xs if x > 0
      y <- foo(x)
  do
      println(x * y)
  end for


trait A :
    def f: Int = 
        val x = 1
        x + 2

trait B extends A{
    override def f = {
        super.f - 1
    }
}

@main def foo = println(Array(1, 2)(2))



