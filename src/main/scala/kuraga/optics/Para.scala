package kuraga.optics

trait Para[X, A, B]:
    self =>
    def get(a: A, x: X): B
    def set(a: A, x: X, b: B): A

    def compose[Y, C](that: Para[Y, B, C]): Para[(X, Y), A, C] = new:
        def get(a: A, xy: (X, Y)): C =
            val (x, y) = xy
            that.get(self.get(a, x), y)

        def set(a: A, xy: (X, Y), c: C): A =
            val (x, y) = xy
            self.set(a, x, that.set(self.get(a, x), y, c))
end Para


object Para:
    def identity[X, A]: Para[X, A, A] = new: 
        def get(a: A, x: X): A = a
        def set(a: A, x: X, b: A): A = b