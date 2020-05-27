package playground.chat.kaivessel
import Eq.GEQ

final case class LS[F[_]](l: List[F[String]])
object LS:  
    given[F[_]](using Semigroup[List[F[String]]]) as  Semigroup[LS[F]] = (a, b) => LS(a.l <+> b.l)
    given View[LS]:
       def nullV[F[_]](container: LS[F]) = container.l.isEmpty


final case class LI[F[_]](l: List[F[Int]])
object LI:
    given[F[_]](using Semigroup[List[F[Int]]]) as Semigroup[LI[F]] = (a, b) => LI(a.l <+> b.l)      
    given View[LI]:
        def nullV[F[_]](container: LI[F]) = container.l.isEmpty  

enum ExampleGADT[c[f[_]]]:
  case S extends ExampleGADT[LS]
  case I extends ExampleGADT[LI]

object ExampleGADT:    
    given Eq[K2, ExampleGADT]:
        def [A[_[_]], B[_[_]]](k: ExampleGADT[A]) isEq (k2: ExampleGADT[B]) = 
            [R] => (eq: GEQ[K2, ExampleGADT, A, B, R]) => k match
            case ExampleGADT.S => k2 match 
                case ExampleGADT.S => eq.Y(ExampleGADT.S, Is.refl[K2, A])
                case ExampleGADT.I => eq.N        
            case ExampleGADT.I => k2 match 
                case ExampleGADT.S => eq.N
                case ExampleGADT.I => eq.Y(ExampleGADT.I, Is.refl[K2, A])
                
    given [TC <: K3](using tcInt: TC[LI], tcString: TC[LS]) as Has[K2, TC, ExampleGADT]:
        def [A[_[_]]](gadt: ExampleGADT[A]) constraintsFor: TC[A] = gadt match
            case ExampleGADT.S => tcString
            case ExampleGADT.I => tcInt
  

final case class ExampleValue[c[_[_]]](get: c[Option]) 

object ExampleValue:
    given [c[f[_]]](using Semigroup[c[Option]]) as Semigroup[ExampleValue[c]] = (a, b) => ExampleValue[c](a.get <+> b.get)

@main def checkVessel() = 
  import ExampleGADT.{S, I}
  val monoidalDMap =
    MonoidalDMap(S -> ExampleValue[LS](LS(List(Option("str"))))) ++
      MonoidalDMap(I -> ExampleValue[LI](LI(List(Option(1)))))

  val vessel =
    Vessel(S -> LS[Option](List(Option("str")))) ++
    Vessel(I -> LI[Option](List(Option(1))))

  println(monoidalDMap: MonoidalDMap[K2, ExampleGADT, ExampleValue])
  println(vessel: Vessel[ExampleGADT, Option])
