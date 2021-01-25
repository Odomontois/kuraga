import playground.chat.Kek


class BuznesErar
class ServesErar

def buznes: BiDyrka[BuznesErar, String] = BiDyrka.impl
def serves(s: String): BiDyrka[ServesErar, Int] = BiDyrka.impl

val x : BiDyrka[ServesErar | BuznesErar, Int] = 
    for
    str  <- buznes
    int  <- serves(str)
    str2 <- buznes
    int2 <- serves(str2)
    yield int + int2

// @main def bydyrka = println(Kek.whatType(x))

enum BiDyrka[+E, +A]:
    case impl

    def flatMap[E1, B](f: A => BiDyrka[E1, B]): BiDyrka[E | E1, B] = impl
    def map[B](f: A => B): BiDyrka[E, B] = impl



