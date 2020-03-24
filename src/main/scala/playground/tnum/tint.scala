package playground.tint



object Lol extends App{
  type ReverseLoop[A, XS <: Tuple] <: Tuple = A match {
    case Unit => XS
    case x *: xs => ReverseRecur[xs, x *: XS]
  }

  type ReverseRecur[A, XS <: Tuple] = XS match { case _ => ReverseLoop[A, XS] }

  type Reverse[A] = ReverseLoop[A, Unit]

  type PlusBit[A, B] = (A, B) match{
    case (true, true)  => (true, true)
    case (true, false) | (false, true) => (false, true)
    case (false, false) => (false, false)
  }

  type PlusTri[A, B, C] <: (Boolean, Boolean) = (A, B, C) match {
    case (false, false, false) => (false, false)
    case (true, false, false) | (false, true, false) | (false, false, true) => (false, true)
    case (true, true, false)  | (true, false, true)  | (false, true, true)  => (true , false)
    case (true, true, true)    => (true, true)
  } 

  type Inc[A <: Tuple] <: Tuple = A match {
    case Unit => true *: Unit
    case t *: as => t match {
      case false => true *: as
      case true => false *: Inc[as]
    }    
  }

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
      case (o1, r) => r *: PlusLoop[as, bs, o1]
    }
  }

  type Plus[A, B] = Reverse[PlusLoop[Reverse[A], Reverse[B], false]]


  // inline def valueAll[XS <: Tuple] <: Tuple= (??? : XS) match {
  //   case _ : Unit => ()
  //   // case (_ : (x *: rest)) => implicit match {
  //   //   case u : ValueOf[x] => u.value *: valueAll[rest]
  //   // }
  // }

  type Fst[A <: Tuple] = A match {
    case x *: y => x
    case Unit   => Unit
  }

  trait ValueAll[T <: Tuple]{
    def value: T
  }

  given ValueAll[Unit]{ def value = () }
  given [X, XS <: Tuple]  (using x: ValueOf[X], xs: ValueAll[XS]) as ValueAll[X *: XS]  { 
    def value = x.value *: xs.value
  }

  def valueAll[T <: Tuple] (using v: ValueAll[T]) : T = v.value
  def printAll[T <: Tuple] (using ValueAll[T]) = println(valueAll[T])



  type R = Plus[(true, true), (false, true)]
  type R1 = PlusTri[true, false, true]


  // printAll[PlusLoop[(true, true), (true, true), true]]

  // println(valueAll[(1, 2, "kek", 4)])

  type Q = Plus[(true, true, true), (true, true)]

  // summon[Q =:= (true, false, true)]
  // println(valueAll[Q])


  

  // println(summon[ValueOf[R]].value)
}