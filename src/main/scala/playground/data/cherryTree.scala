package playground

trait Monoid[A]:
    def empty: A
    def (a: A) |+| (b: A): A

object Monoid:
    def empty[A](using m: Monoid[A]): A  = m.empty

    given Monoid[Int]:
        def empty = 0
        def (x: Int) |+| (y: Int) = x + y

trait Measured[A, R]:
    def (a: A) measure: R

object Measured:
    def of [A, R](a: A)(using Measured[A, R]): R = a.measure    

    given [A] as Measured[A, A]:
        def (a: A) measure: A = a

    given [A, R](using Measured[A, R], Monoid[R]) as Measured[(A, A), R]:
        def (a: (A, A)) measure: R = a._1.measure |+| a._2.measure

    given [A, R](using Measured[A, R], Monoid[R]) as Measured[Option[A], R]:
        def (a: Option[A]) measure: R = a.fold(Monoid.empty)(_.measure)

enum CherryTree[A]:    
    case Empty()
    case One(a: A)
    case Branch(left: Option[A], mid: CherryTree[(A, A)], right: Option[A])

    def :+(a: A): CherryTree[A] = this match 
        case Empty()               => One(a)
        case One(b)                => Branch(None, One((b, a)), None)
        case Branch(l, m, None)    => Branch(l, m, Some(a))
        case Branch(l, m, Some(b)) => Branch(l, m :+ (b, a), None)

    def +:(a: A): CherryTree[A] = this match 
        case Empty()               => One(a)
        case One(b)                => Branch(None, One((a, b)), None)
        case Branch(None, m, r)    => Branch(Some(a), m, r)
        case Branch(Some(b), m, r) => Branch(None, (b, a) +: m, r)

object CherryTree:
    def apply[A](a: A*): CherryTree[A] = a.foldLeft(CherryTree.Empty())(_ :+ _)

    given measured[A, R](using Measured[A, R], Monoid[R]) as Measured[CherryTree[A], R]:
        def (tree: CherryTree[A]) measure = tree match 
            case CherryTree.Empty()          => Monoid.empty
            case CherryTree.One(a)           => a.measure
            case CherryTree.Branch(l, m, r)  => 
                Measured.of[R = R](l) |+| Measured.of[R = R](m) |+| Measured.of[R = R](r)

@main def cherryTreeMain() = 
    val tree = CherryTree(1, 2, 3, 4, 5, 6, 7)
    println(tree)
    println(tree.measure)

