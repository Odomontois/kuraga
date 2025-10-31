package amst.dependent

class Foo
class Bar {
  def aaa(f: (foo: Foo) => Foo^) = ()
}

class Kek(foo: Foo^, fooer: (foo: Foo) => Foo^)
object check:
  val x: Foo => Foo^ = x => x

  Bar().aaa(a => x(a))
  Kek(Foo(), a => x(a))
