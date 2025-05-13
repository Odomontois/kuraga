object Solution:
    import Matrix.Matrix
    val Transform = Matrix.tabulate[26, 26]:
        case (25, 0 | 1) => 1
        case (x, y) if y == x + 1 => 1
        case _ => 0

    def counts(s: String) = 
        val cm = s.groupMapReduce(_ - 'a')(_ => 1L)(_ + _)
        Matrix.tabulate[1, 26]((_, i) => cm.getOrElse(i, 0L))

    def lengthAfterTransformations(s: String, t: Int): Int = 
        ((counts(s) * Transform.pow(t)).toArray.sum % Matrix.Mod).toInt

import scala.compiletime.constValue

object Matrix:
    opaque type Matrix[M <: Int, N <: Int] = Array[Long]
    inline val Mod = 1_000_000_007

    inline def tabulate[M <: Int, N <: Int](inline f: (Int, Int) => Long): Matrix[M, N] = 
        val N = constValue[N]
        Array.tabulate(constValue[M] * N)(i => f(i / N, i % N))

    inline def ident[N <: Int]: Matrix[N, N] = tabulate((i, j) => if i == j then 1 else 0)

    inline def sum(inline n: Int)(inline f: Int => Long): Long = 
        def sumIter(i: Int, acc: Long): Long = if i == n then acc else sumIter(i + 1, acc + f(i))
        sumIter(0, 0)

    extension [M <: Int, N <: Int] (xs: Matrix[M, N]) 
        inline def show: String = 
            val N = constValue[N]
            xs.grouped(N).map(_.mkString(" ")).mkString("\n")
        inline def toArray: Array[Long] = xs
        inline def apply(i: Int, j: Int): Long = xs(i * constValue[N] + j)
        inline def *[T <: Int](ys: Matrix[N, T]): Matrix[M, T] = 
            Matrix.tabulate[M, T]: 
                (i, j) => sum(constValue[N])(k => xs(i, k) * ys(k, j) % Mod) % Mod

    extension [N <: Int](m: Matrix[N, N])
        inline def pow(x: Int): Matrix[N, N] = 
            def powIter(p: Matrix[N, N], acc: Matrix[N, N], y: Int): Matrix[N, N] = 
                if y == 1 then acc * p else powIter(p * p, if y % 2 == 1 then acc * p else acc, y / 2)
            powIter(m, ident, x)


Solution.lengthAfterTransformations("abcyy", 2) // 2
Solution.lengthAfterTransformations("azbk", 1) // 2
Solution.lengthAfterTransformations("z" * 10, 30) // 3
Solution.Transform.pow(2).show // 2
