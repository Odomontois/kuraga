trait Named:
  def name: String

  def me: Named
end Named

abstract class Foo extends Named:
  def name = "foo"

  def me: Foo = this

  def foo(x: String): String
end Foo

class Names(xs: List[Named]):
  def mkString = xs.iterator.map(_.me.name).mkString(",")

object Names:
  def single[T <: Named](t: T): Names = Names(List(t))

@main def Repro() =
  val names = Names.single[Foo](x => x)
  println(names.mkString)
