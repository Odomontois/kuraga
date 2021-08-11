enum Tree[+A] 
    case Leaf[+A](l: Tree[A], n: A, r: Tree[A]) extends Tree[A]
    case Empty 
   
    def foldRight[B](end: B)(f: (A, B) => B): B = 
        def (tree: Tree[A]) <<: (b: B): B = tree match 
            case Empty => end
            case Leaf(l, a, r) => f(a, r <<: l <<: b)        
        
        this <<: end
        
import Tree._

val a = Leaf(Leaf(Empty, 1, Empty),2,Leaf(Empty, 3, Empty))
val x = a.foldRight(0)(_ + _)
println(x)