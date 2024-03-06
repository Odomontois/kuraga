package playground.indexed.freeCat

sealed trait IndexedList[Event[_, _], Begin, End] {
    def +:[Prev](head: Event[Prev, Begin]): IndexedList[Event, Prev, End] = +&:(head, this)
}

final case class INil[Event[_, _], State]() extends IndexedList[Event, State, State]

final case class +&:[Event[_, _], Begin, Middle, End](
    head: Event[Begin, Middle],
    tail: IndexedList[Event, Middle, End],
) extends IndexedList[Event, Begin, End]

/** все возможные состояния перечислены как типы мы объявили их как final классы, а не как абстрактные типы, чтобы
  * компилятор понимал при паттерн матчинге различия, например, что Start != End
  */
abstract final class Start
abstract final class A
abstract final class B
abstract final class C
abstract final class D
abstract final class End

// "колчан" - события с типами состояний в качестве индексов
// final обязательны, иначе скала тупит и принимает в патмате что-угодно
sealed trait Evt[From, To]
final case class GoToA(info: StartInfo)    extends Evt[Start, A]
final case class GoToB(info: ContinueInfo) extends Evt[A, B]
final case class LoopB(info: LoopInfo)     extends Evt[B, B]
final case class GoToC(info: SwitchC)      extends Evt[B, C]
final case class GoToD(info: SwitchD)      extends Evt[B, D]
final case class FromC(info: EndC)         extends Evt[C, End]
final case class FromD(info: EndD)         extends Evt[D, End]

// разрозненная информация, не размечена состояниями
case class StartInfo(info: String)
case class ContinueInfo(info: String)
case class LoopInfo(info: String)
case class SwitchC(info: String)
case class SwitchD(info: String)
final case class EndC(info: String)
case class EndD(info: String)

// представим, что какую-то такую информацию мы хотим собрать
case class CollectedInfo(
    start: StartInfo,
    continue: ContinueInfo,
    loop: Vector[LoopInfo],
    corD: Either[(SwitchC, EndC), (SwitchD, EndD)],
)

// собираем информацию, в scala 2 будет больше бойлерплейта из-за проблем с GADT
def collectInfo(chain: IndexedList[Evt, Start, End]): CollectedInfo = {
    def accumBs(
        chain: IndexedList[Evt, B, End],
        infoA: StartInfo,
        infoB: ContinueInfo,
        bs: Vector[LoopInfo] = Vector.empty[LoopInfo],
    ): CollectedInfo = chain match {
        case LoopB(info) +&: rest  => accumBs(rest, infoA, infoB, bs :+ info)
        case GoToC(infoC) +&: rest => branchC(rest, infoA, infoB, bs, infoC)
        case GoToD(infoD) +&: rest => branchD(rest, infoA, infoB, bs, infoD)
    }

    def branchC(
        chain: IndexedList[Evt, C, End],
        infoA: StartInfo,
        infoB: ContinueInfo,
        bs: Vector[LoopInfo],
        infoC: SwitchC,
    ): CollectedInfo = chain match {
        case FromC(info) +&: INil() => CollectedInfo(infoA, infoB, bs, Left(infoC -> info))
    }

    def branchD(
        chain: IndexedList[Evt, D, End],
        infoA: StartInfo,
        infoB: ContinueInfo,
        bs: Vector[LoopInfo],
        infoD: SwitchD,
    ): CollectedInfo = chain match {
        case FromD(info) +&: INil() => CollectedInfo(infoA, infoB, bs, Right(infoD -> info))
    }

    chain match {
        case GoToA(infoA) +&: GoToB(infoB) +&: rest => accumBs(rest, infoA, infoB)
    }
}

@main def lol() = {
    val chain1 = GoToA(StartInfo("start")) +:
        GoToB(ContinueInfo("continue")) +:
        LoopB(LoopInfo("loop once")) +:
        LoopB(LoopInfo("loop twice")) +:
        GoToC(SwitchC("switch")) +:
        FromC(EndC("end")) +:
        INil()

    val chain2 = GoToA(StartInfo("start")) +:
        GoToB(ContinueInfo("continue")) +:
        GoToD(SwitchD("switch")) +:
        FromD(EndD("end")) +:
        INil()

    println(collectInfo(chain1))
    println(collectInfo(chain2))
}
