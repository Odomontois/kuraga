package playground

import kuraga.Monoid
import kuraga.Semigroup.Sum

trait Measured[A, R]:
  extension (a: A) def measure: R

object Measured:
  def of[A, R](a: A)(using Measured[A, R]): R = a.measure

  given [A: Monoid]: Measured[A, A] with
    extension (a: A) def measure: A = a

  given [A, R](using Measured[A, R], Monoid[R]): Measured[(A, A), R] with
    extension (a: (A, A)) def measure: R = a._1.measure |+| a._2.measure

  given [A, R](using Measured[A, R], Monoid[R]): Measured[Option[A], R] with
    extension (a: Option[A]) def measure: R = a.fold(Monoid.default)(_.measure)

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

  given [A, R](using Measured[A, R], Monoid[R]): Measured[CherryTree[A], R] with
    extension (tree: CherryTree[A])
      def measure = tree match
        case CherryTree.Empty()         => Monoid.default
        case CherryTree.One(a)          => a.measure
        case CherryTree.Branch(l, m, r) => Measured.of(l) |+| m.measure |+| Measured.of(r)

@main def cherryTreeMain() =
    val tree = CherryTree[Sum[Int]](1, 2, 3, 4, 5, 6, 7)
    println(tree)
    println(tree.measure)
