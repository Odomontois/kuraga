package playground.typeclasses.derivation


case class Foo(x: String, y: Int) derives Show
case class Bar(hd: String, next: Option[Bar]) derives Show

enum Tree[+A] derives Show:
    case Leaf
    case Branch(value: A, left: Tree[A], right: Tree[A])

object Tree:
    def apply[A](a: A, left: Tree[A] = Leaf, right: Tree[A] = Leaf) = Branch(a, left, right)

@main def check() = 
    println(Foo("lol", 1).shown)
    println(Bar("1", Some(Bar("2", None))).shown)
    println(Tree(1, Tree(2), Tree(3)).shown)