package kuraga.tint

type ReverseLoop[A, XS <: Tuple] <: Tuple = A match {
  case Unit => XS
  case x *: xs => ReverseLoopRecur[xs, x *: XS]
}

type ReverseLoopRecur[A, XS <: Tuple] = XS match {
  case _ => ReverseLoop[A, XS]
}

type Reverse[A] = ReverseLoop[A, Unit]

type PlusBit[A, B] = (A, B) match{
  case (true, true)  => (true, true)
  case (true, false) | (false, true) => (false, true)
  case (false, false) => (false, false)
}

type PlusTri[A, B, C] = (A, B, C) match {
  case (false, false, false) => (false, false)
  case (true, false, false) | (false, true, false) | (false, false, true) => (false, true)
  case (true, true, false)  | (true, false, true)  | (false, true, true)  => (true , false)
  case (true, true, true)    => (true, true)
} 

type Inc[A] <: Tuple = A match {
  case Unit => true *: Unit
  case false *: as => true *: as
  case true *: as => false *: IncRec[as]
}
type IncRec[A] = A match { case _ => Inc[A]}

type IncT[A <: Tuple, O] <: Tuple = O match {
  case false => A
  case true => Inc[A]
}

type PlusLoop[A <: Tuple, B <: Tuple, O] <: Tuple = (A, B) match {
  case (Unit, Unit)       => O match {
    case true => (true *: Unit)
    case false => Unit
  }
  case (Unit, _) => IncT[B, O]
  case (_, Unit) => IncT[A, O]
  case (a *: as, b *: bs) => PlusTri[a, b, O] match {
    case (o1, r) => r *: PlusRec[as, bs, o1]
  }
}

type PlusRec[A <: Tuple, B <: Tuple, O] <: Tuple = A match{  case _ =>  PlusLoop[A, B, O] }

type Plus[A, B] = Reverse[PlusLoop[Reverse[A], Reverse[B], false]]


// inline def valueAll[XS <: Tuple] <: Tuple= (??? : XS) match {
//   case _ : Unit => ()
//   // case (_ : (x *: rest)) => implicit match {
//   //   case u : ValueOf[x] => u.value *: valueAll[rest]
//   // }
// }

trait ValueAll[T <: Tuple]{
  def value: T
}

implied for ValueAll[Unit]{ def value = () }
implied [X, XS <: Tuple] given (x: ValueOf[X], xs: ValueAll[XS]) for ValueAll[X *: XS] { def value = x.value *: xs.value }

def valueAll[T <: Tuple] given (v: ValueAll[T]) : T = v.value

object Lol extends App{
  type R = Plus[(true, true), (false, true)]

  // println(valueAll[(1, 2, "kek", 4)])

  type Q = Plus[(true, true, true), (true, true)]

  // the[Q =:= (true, false, true)]
  println(valueAll[Q])


  

  // println(the[ValueOf[R]].value)
}