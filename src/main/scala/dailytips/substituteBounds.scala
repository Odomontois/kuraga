package dailytips
def coerceCo[
    High,
    Low <: High,
    F[+_ >: Low <: High],
    X >: Low <: High,
    Y >: Low <: High
](f: F[X])(using ev: X <:< Y): F[Y] =
    type F1[+A] = F[A & High | Low]

    ev.substituteCo[F1](f)
end coerceCo

def coerceContra[
    High,
    Low <: High,
    F[-_ >: Low <: High],
    X >: Low <: High,
    Y >: Low <: High
](f: F[Y])(using ev: X <:< Y): F[X] =
    type F1[-A] = F[A & High | Low]
    ev.substituteContra[F1](f)
end coerceContra

class Foo:
    def update(x: Int, y: Int, z: Int): Unit = 
        println(s"update($x, $y, $z)")

@main def chikpibaru() =

    val person: ({val name: String; val age: Int}) = 
        new AnyRef {val name = "tarao"; val age = 3}    

    val foo = Foo()

    foo(1, 2) = 3
