package playground.chat

import cats.*
import cats.syntax.all.given

package lolDiscusionShow:

    class Foo
    object Foo:
        given Show[Foo] = _ => "I'm Foo"
    class Bar extends Foo
    class Baz extends Foo
    object Baz:
        given Show[Baz] = _ => "I'm Baz"

    @main def kek() =
        println(show" foo: ${Foo()}, bar: ${Bar()}, baz: ${Baz()}")
end lolDiscusionShow