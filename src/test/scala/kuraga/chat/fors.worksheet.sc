class Lol(val ops: String = "$"):
    def flatMap(f: Unit => Lol) = Lol(ops + ".flatMap(" + f(()).ops + ")")
    def map(f: Unit => Unit) = Lol(ops + ".map")

val a = 
    for x <- Lol()
        y <- Lol()
        z <- Lol()
    yield z

a.ops