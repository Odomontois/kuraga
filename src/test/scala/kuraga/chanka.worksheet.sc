trait Base:
    def print(): Unit


class BaseImpl(val x: Int) extends Base:
    def print() = println(x)

class Derived(x: Base) extends Base:
    export x.*