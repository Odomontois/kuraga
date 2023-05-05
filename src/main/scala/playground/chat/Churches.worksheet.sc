import com.sourcegraph.semanticdb_javac.Semanticdb.WithType
trait Nat:
    def apply[A](f: A => A): A => A
    def unary_~[A]: (A => A) => A => A = apply

object Nat:
    def apply(f: (ty: { type T }) => (ty.T => ty.T) => ty.T => ty.T): Nat =
        new { def apply[A](s: A => A): A => A = f(().asInstanceOf[{ type T = A }])(s) }
def pow = (x: Nat) => (y: Nat) => Nat(_ => y(~x))

def zero: Nat = Nat(_ => s => z => z)

def succ = (n: Nat) => Nat(_ => s => z => s(n(s)(z)))

def two   = succ(succ(zero))
def three = succ(two)

println(pow(two)(three)[Int](_ + 1)(0))
println(pow(three)(two)[Int](_ + 1)(0))

trait HasType:
    type T

    def first: T
    def second: T

object IsInt    extends HasType:
    type T = Int
    val first  = 1
    val second = 2
object IsString extends HasType:
    type T = String
    val first  = "uno"
    val second = "dos"

def asList(x: HasType): List[x.T] = List(x.first, x.second)

val x: List[Int]    = asList(IsInt)
val y: List[String] = asList(IsString)

f"${123}%h"