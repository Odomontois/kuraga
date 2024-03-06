package playground.typeclasses.derivation
import scala.compiletime.{summonFrom, erasedValue, summonInline}
import scala.deriving.Mirror

trait Show[A]:
    def show(a: A): String
    extension (a: A) def shown: String = show(a)

object Show:
    given Show[String]               = x => x
    given Show[Int]                  = _.toString
    given [A: Show]: Show[Option[A]] = {
        case None    => "<none>"
        case Some(x) => x.shown
    }

    private inline def showTuple[ET <: Tuple, EL <: Tuple](xs: ET): List[String] =
        inline xs match
            case ht: (h *: t) =>
                inline erasedValue[EL] match
                    case _: (hl *: tl) =>
                        val hl   = summonInline[ValueOf[hl]].value
                        val sh   = summonInline[Show[h]]
                        val ht1 : (h *: t) = ht
                        val hs   = sh.show(ht1.head)
                        val head = s"$hl: $hs"
                        head :: showTuple[t, tl](ht1.tail)

            case _: EmptyTuple => Nil

    private inline def productShow[A <: Product](using m: Mirror.ProductOf[A]): Show[A] = new:
        def show(a: A) =
            showTuple[m.MirroredElemTypes, m.MirroredElemLabels](Tuple.fromProductTyped(a)).mkString("{", ",", "}")

    private inline def coproductShows[A, ET <: Tuple](idx: Int): Vector[Show[A]] = inline erasedValue[ET] match
        case ht: (h *: t) =>
            type H = h
            val g: Show[H] = summonFrom {
                case g: Show[H] => g
                case _          => derived[H]
            }
            g.asInstanceOf[Show[A]] +: coproductShows[A, t](idx + 1)

        case EmptyTuple => Vector()

    inline def derived[A]: Show[A] = summonFrom {
        case m: Mirror.ProductOf[A with Product] =>
            val tt = summonInline[A =:= A with Product]
            tt.substituteContra[Show](productShow[A with Product])

        case ms: Mirror.SumOf[A] =>
            val shows = coproductShows[A, ms.MirroredElemTypes](0)
            new Show[A]:
                def show(a: A) = shows(ms.ordinal(a)).show(a)
    }
end Show
