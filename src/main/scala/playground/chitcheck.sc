trait Functya[F[_]]:
    def [A, B] (fa: F[A]) map (f: A => B) : F[B]

given Functya[List] with
    def [A, B] (fa: List[A]) map (f: A => B) : List[B] = 
        fa.map(f)


def lol[F[_]: Functya](x: F[Int]): F[String] = x.map(i => (i + 1).toString)

lol(List(1, 2, 3))


