// package playground.chat.kaivessel
// import Eq.GEQ
// import GEQ.{Y, N}


// final case class LS[F[_]](l: List[F[String]])
// final case class LI[F[_]](l: List[F[Int]])

// enum ExampleGADT[c[f[_]]]:
//   case S extends ExampleGADT[LS]
//   case I extends ExampleGADT[LI]

// object ExampleGADT:    
//   given Eq[K2, ExampleGADT]:
//     def [A[_[_]], B[_[_]]](k: ExampleGADT[A]) isEq (k2: ExampleGADT[B]): GEQ[K2, ExampleGADT, A, B] = k match 
//       case ExampleGADT.S => k2 match 
//         case ExampleGADT.S => Y(ExampleGADT.S)
//         case ExampleGADT.I => N[K2, ExampleGADT, A, B]()        
//       case ExampleGADT.I => k2 match 
//         case ExampleGADT.S => N[K2, ExampleGADT, A, B]()
//         case ExampleGADT.I => Y(ExampleGADT.I)

// final case class ExampleValue[c[f[_]]](get: c[Option])