import cats.conversions.variance
import kuraga.A
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

// val xs = List.range(1, 5)

// def foo(x: Int) = 
//     List(x, -x)

// for x <- xs if x > 0
//     y <- foo(x)
// yield x * y

// for x <- xs if x > 0
//     y <- foo(x)
// do
//     println(x * y)

// import scala.collection.mutable.ArrayBuffer
// class Table:
//     val rows = new ArrayBuffer[Row]
//     def add(r: Row): Unit = rows += r
//     override def toString = rows.mkString("Table(", ", ", ")")

// class Row:
//     val cells = new ArrayBuffer[Cell]
//     def add(c: Cell): Unit = cells += c
//     override def toString = cells.mkString("Row(", ", ", ")")

// case class Cell(elem: String)

// def table(init: Table ?=> Unit) =
//     given t: Table = Table()
//     init
//     t

// def row(init: Row ?=> Unit)(using t: Table) =
//     given r: Row = Row()
//     init
//     t.add(r)

// def cell(str: String)(using r: Row) =
//     r.add(new Cell(str))

// table {
//     row {
//         cell("top left")
//         cell("top right")
//     }
//     row {
//         cell("bottom left")
//         cell("bottom right")
//     }
// }


// trait Arith:
//     type A
//     def variable(name: String): A
//     def plus(x: A, y: A): A
//     def times(x: A, y: A): A

//     extension (name: String) def v: A = variable(name)
//     extension (x: A)
//         infix def + (y: A): A = plus(x, y)
//         infix def * (y: A): A = times(x, y)

// import scala.language.dynamics

// object Arith:
//     trait ArithRes[Res] extends Arith:
//         type A = Res
//     given string: ArithRes[String] with
//         def variable(name: String) = name
//         def plus(x: String, y: String) = s"($x + $y)"
//         def times(x: String, y: String) = s"($x * $y)"

//     given calc(using m: Map[String, BigInt]): ArithRes[BigInt] with
//         def variable(name: String) = m.getOrElse(name, 0)
//         def plus(x: BigInt, y: BigInt) = x + y
//         def times(x: BigInt, y: BigInt) = x * y


// object V extends Dynamic:
//     def selectDynamic(name: String)(using A: Arith): A.A = A.variable(name)
// class Formula(body: (l: Arith) ?=> l.A) extends Dynamic:
//     override def toString: String = body(using Arith.string)
//     def applyDynamicNamed(apply: "apply")( vars: (String, BigInt)*): BigInt = 
//         given m: Map[String, BigInt] = vars.toMap
//         body(using Arith.calc)

// val formula = Formula((V.x + V.y) * (V.z + V.x))

// formula(x = 1, y = 2, z = 3)
