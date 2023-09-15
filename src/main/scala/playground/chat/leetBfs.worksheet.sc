def validPath(n: Int, edges: Array[Array[Int]], source: Int, destination: Int): Boolean = {
    val graph                                        = edges.view
        .flatMap { case Array(x, y) => Array(x -> y, y -> x) }
        .groupMap(_._1)(_._2)
    def bfs(q: Vector[Int], seen: Set[Int]): Boolean = q match {
        case `destination` +: _ => true
        case i +: rest          =>
            val next = graph.getOrElse(i, Nil).filterNot(seen)
            bfs(rest ++ next, seen ++ next)
        case _                  => false
    }
    bfs(Vector(source), Set(source))
}
