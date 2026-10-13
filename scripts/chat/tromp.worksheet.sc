object Kek {
    opaque type Foo[A, B] = (A, A => B)
}

class Bar(val x: Kek.Foo[?, ?])
