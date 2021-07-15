import cats.kernel.instances.ListMonoid
1 + 2

// trait Base:
//     def name: String
//     def age: Int
//     final def me = s"age:$name name:$name"

// class Derived(b1: Base, b2: Base) 
//     extends Base:
//     export b1.name
//     export b2.age


// val d = Derived(
//     new{
//         def name = "Oleg"
//         def age = 32
//     },
//     b2 = new :
//             def name = "Katya"
//             def age  = 25
    
// )

// trait Foo(val x: String)

// trait Bar extends Foo:
// 	val y = x.toUpperCase

// (new Bar with Foo("lol"){}).y


trait Monoid[A]:
    def combine(x: A, y: A): A
    def empty: A
    extension (x: A) 
    	infix def |+|(y: A): A = combine(x, y)

// def empty[A](using A: Monoid[A]): A = A.empty

// extension [A](xs: Vector[A]) 
//     def collectWith[B: Monoid](f: A => B): B = 
//         xs.view.map(f).foldLeft(empty)(_ |+| _) 

//     def collect(using Monoid[A]): A = collectWith(x => x)

// object Monoid:
//     given Monoid[String] with
//         def combine(x: String, y: String) = x + y
//         def empty = ""

//     given Monoid[Int] with 
//         def combine(x: Int, y: Int) = x + y
//         def empty = 0
       
//     given [A]: Monoid[List[A]] with
//         def combine(x: List[A], y: List[A]) = x ::: y
//         def empty = Nil
    
//     given [A, B](using A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] with
//         def combine(x: (A, B), y: (A, B)) = 
//             (A.combine(x._1, y._1), B.combine(x._2, y._2))
//         def empty = (A.empty, B.empty)



// tr)ait Formula[A]:
//     def plus(