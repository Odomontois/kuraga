import scala.compiletime.ops.int.*

sealed trait HList:
    type Self <: HList

    import HList.{HNil, &::}

    def &::[H](h: H): H &:: Self =
        val a1 = h.asInstanceOf[AnyRef]
        this match
            case HNil             => HList.&::(IArray(a1))
            case HList.&::(elems) => HList.&::(a1 +: elems)

end HList

object HList:

    case object HNil extends HList:
        type Self = HNil.type

    case class &::[H, T <: HList] private[HList] (private[HList] elems: IArray[AnyRef]) extends HList:
        type Self = H &:: T
        def apply[i <: Int & Singleton](i: i): Index[Self, i] =
            elems(i).asInstanceOf[Index[Self, i]]

    type HNil = HNil.type

    type Index[H <: HList, i <: Int] = H match
        case h &:: t =>
            i match
                case 0   => h
                case Int => Index[t, i - 1]

end HList

import HList.{HNil, &::}

val y = "a" &:: 1 &:: 4.5 &:: true &:: "gdsdf" &:: 3 &:: HNil

y(0): String
y(1)
y(2)
y(3)
