enum GADT1:
    case Pair[A](first: A, second: A) extends GADT1

enum GADT2[A]:
    case IsInt(first: Int, second: Int)          extends GADT2[Int]
    case IsString(first: String, second: String) extends GADT2[String]

    def first: A
    def second: A

trait TM:
    type T
    def first: T
    def second: T

object TM:
    case class IsInt(first: Int, second: Int) extends TM:
        type T = Int

    case class IsString(first: String, second: String) extends TM:
        type T = String
end TM

case class Iso[A, B](from: A => B, to: B => A)

Iso[GADT1, TM](
  { case p: GADT1.Pair[a] =>
      new TM {
          type T = a
          val first  = p.first
          val second = p.second
      }
  },
  tm => GADT1.Pair(tm.first, tm.second)
)

def iso2[A] = Iso[GADT2[A], TM { type T = A }](
  {
      case GADT2.IsInt(first, second)    => TM.IsInt(first, second)
      case GADT2.IsString(first, second) => TM.IsString(first, second)
  },
  {
      case TM.IsInt(first, second)    => GADT2.IsInt(first, second).asInstanceOf[GADT2[A]]
      case TM.IsString(first, second) => GADT2.IsString(first, second).asInstanceOf[GADT2[A]]
  }
)

f"${1}"
