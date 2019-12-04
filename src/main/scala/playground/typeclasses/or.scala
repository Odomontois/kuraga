package playground.typeclasses

trait Foo
trait Bar

object Or {
    def ponv (given Bar | Foo) = 1

    def lol (given Bar) = ponv

    def kek(given Foo) = ponv
}