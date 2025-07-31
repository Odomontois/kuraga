trait A:
    def foo = "A"

trait B:
    def foo = "B"

trait C extends B, A:
    override def foo = "C"

    def lol = super[B].foo
    def kek = super[A].foo

val b = new C {}

b.foo
b.lol
b.kek

(((-10000 + 5) % 10000 + 10000) % 10000)

