package kuraga

trait Base:
    def name: String
    def name(full: Boolean): String
    def age: Int
    final def me = s"age:$age name:$name fullName: ${name(true)}"

class Derived(b1: Base, b2: Base) 
    extends Base:
    export b2.age
    export b1.name


val d = Derived(
    new:
        def name = "Oleh"
        def name(full: Boolean) = "Olga"
        def age = 32,
    new:
        def name = "Katya"
        def name(full: Boolean) = name
        def age  = 27
)

@main def lol() = println(d.me)