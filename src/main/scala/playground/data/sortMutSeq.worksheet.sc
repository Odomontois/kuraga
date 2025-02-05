import java.util.Comparator
val x1 = Array(1, 5, 2, 4, 6)
val x2 = Array(2, 6, 3, 5, 1)

val x = new java.util.AbstractList[(Int, Int)]:
    def get(index: Int) = (x1(index), x2(index))
    def size(): Int     = x1.size

    override def set(index: Int, element: (Int, Int)) =
        val res = (x1(index), x2(index))
        x1(index) = element._1
        x2(index) = element._2
        res

x.sort(Comparator.comparing { case (x, y) => x + y })
x
