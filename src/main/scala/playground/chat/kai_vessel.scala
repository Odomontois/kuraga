package playground.chat
//port of https://scastie.scala-lang.org/1QH1bFGSRcSkcGavkm0Zbw 

import Eq.{GEQ}
import GEQ.{Y, N}
import Vessel.Vessel
import MonoidalDMap.MonoidalDMap

final case class LS[F[_]](l: List[F[String]])
final case class LI[F[_]](l: List[F[Int]])

enum ExampleGADT[c[f[_]]]:
  case S extends ExampleGADT[LS]
  case I extends ExampleGADT[LI]

final case class ExampleValue[c[f[_]]](get: c[Option])

// hs:GCompare
trait Eq[K <: K3] :
  def [A[_[_]], B[_[_]]](k: K[A]) isEq (k2: K[B]): GEQ[K, A, B]

object Eq:
  enum GEQ[K <: K3, A[_[_]], B[_[_]]]:
    case Y[K <: K3, A[_[_]]](res: K[A]) extends GEQ[K, A, A]
    case N() extends GEQ[K, A, B]

  given Eq[ExampleGADT]:
    def [A[_[_]], B[_[_]]](k: ExampleGADT[A]) isEq (k2: ExampleGADT[B]): GEQ[ExampleGADT, A, B] = k match 
      case ExampleGADT.S => k2 match 
        case ExampleGADT.S => Y(ExampleGADT.S)
        case ExampleGADT.I => N()        
      case ExampleGADT.I => k2 match 
        case ExampleGADT.S => N()
        case ExampleGADT.I => Y(ExampleGADT.I)
        
type K3 = [c[f[_]]] =>> Any
type K2 = [f[_]] =>> Any

sealed trait DPair[K <: K3, +V <: K3]:
  type A[_[_]]
  val key: K[A]
  val value: V[A]
  final def extract[B[_[_]]](k: K[B])(using Eq[K]): Option[V[B]] =  key isEq k match 
    case Y(_) => Some(value)
    case _    => None

object DPair:
  given [K <: K3, V <: K3, C[f[_]]] as Conversion[(K[C], V[C]), DPair[K, V]] = tup => 
    case class dpair(key: K[C], value: V[C]) extends DPair[K, V]:
      type A[f[_]] = C[f]
    dpair(tup._1, tup._2)  

trait DGet[K <: K3, +V <: K3]:
  def get[A[_[_]]](k: K[A])(using Eq[K]): Option[V[A]]

trait DList[K <: K3, +V <: K3]:
  def toList: List[DPair[K, V]]

sealed trait DMap[K <: K3, +V <: K3] extends DGet[K, V] with DList[K, V]:  
  def set[V1 >: V <: K3](kv: DPair[K, V1])(using Eq[K]): DMap[K, V1]
  // right-biased of course
  def ++[V1 >: V <: K3](that: DMap[K, V1])(using Eq[K]): DMap[K, V1]
  def --[V1 >: V <: K3](that: DMap[K, V1])(using Eq[K]): DMap[K, V]  

object DMap:
  def empty[K <: K3, V <: K3]: DMap[K, V] = apply()
  def apply[K <: K3, V <: K3](elems: DPair[K, V]*): DMap[K, V] = 
    case class dmap[V1 >: V <: K3](elems: Seq[DPair[K, V1]]) extends DMap[K, V1] :
      def get[A[_[_]]](k: K[A])(using Eq[K]) = elems.collectFirst(Function.unlift((_: DPair[K, V1]).extract(k)))      
      def ++[V2 >: V1 <: K3](that: DMap[K, V2])(using Eq[K])  = dmap((this -- that).toList ++ that.toList)      
      def --[V2 >: V1 <: K3](that: DMap[K, V2])(using Eq[K])  = dmap(elems.filter(e => that.get(e.key).isEmpty))      
      def set[V2>: V1 <: K3](kv: DPair[K, V2])(using Eq[K])  = this ++ DMap(kv)
      def toList: List[DPair[K, V1]]          = elems.toList    
    dmap(elems)

// hs:ArgDict, Has
trait Has[TC <: K3, GADT <: K3]:
  def [A[_[_]]](gadt: GADT[A]) constraintsFor: TC[A]

object Has:
  given [TC <: K3](using tcInt: TC[LI], tcString: TC[LS]) as Has[TC, ExampleGADT]:
    def [A[_[_]]](gadt: ExampleGADT[A])constraintsFor: TC[A] = gadt match
        case ExampleGADT.S => tcString
        case ExampleGADT.I => tcInt

// hs:Has'
type Has0[TC[_], V <: K3, GADT <: K3] = Has[[A[_[_]]] =>> TC[V[A]], GADT]

trait Semigroup[A]:
  def combine(a: A, b: A): A
  def (a: A) <+> (b: A): A = combine(a, b)

object Semigroup:
  given Semigroup[Int]     = _ + _
  given Semigroup[String]  = _ + _
  given[A] as Semigroup[List[A]] = _ ++ _
  given[A] as Semigroup[Option[A]] = _ orElse _
  given [c[f[_]]](using Semigroup[c[Option]]) as Semigroup[ExampleValue[c]] =
    (a, b) => ExampleValue[c](a.get <+> b.get)
  given[F[_]](using Semigroup[List[F[Int]]]) as Semigroup[LI[F]]    = (a, b) => LI(a.l <+> b.l)
  given[F[_]](using Semigroup[List[F[String]]]) as  Semigroup[LS[F]] = (a, b) => LS(a.l <+> b.l)

object MonoidalDMap:
  opaque type MonoidalDMap[K <: K3, +V <: K3] <: DGet[K, V] with DList[K, V] = DMap[K, V]  

  def empty[K <: K3, V <: K3]: MonoidalDMap[K, V]                      = DMap.empty[K, V]
  def apply[K <: K3, V <: K3](elems: DPair[K, V]*): MonoidalDMap[K, V] = DMap(elems: _*)

  extension ops on [K <: K3, V <: K3](self: MonoidalDMap[K, V]):
    def ++(that: MonoidalDMap[K, V])(using Eq[K], Has0[Semigroup, V, K]): MonoidalDMap[K, V] = 
      val luniq = self -- that
      val runiq = that -- self
      val intersection: List[DPair[K, V]] = 
        for 
          e  <- self.toList
          v2 <- that.get(e.key)
          v1 = e.value
          sg = e.key.constraintsFor
        yield e.key -> sg.combine(v1, v2)      
      
      MonoidalDMap(luniq.toList ++ runiq.toList ++ intersection: _*)   
    
    def set(kv: DPair[K, V])(using Eq[K]): MonoidalDMap[K, V]         = self.set(kv)
    def --(that: MonoidalDMap[K, V])(using Eq[K]): MonoidalDMap[K, V] = self -- that     

trait View[c[f[_]]]:
  def nullV[F[_]](container: c[F]): Boolean
  final def collapseNullV[F[_]](container: c[F]): Option[c[F]] = if (nullV(container)) None else Some(container)

object View:
  given View[LS]:
    def nullV[F[_]](container: LS[F]) = container.l.isEmpty
  
  given View[LI]:
    def nullV[F[_]](container: LI[F]) = container.l.isEmpty  

type FlipAp[G[_]] = [c[f[_]]] =>> c[G]

sealed trait VSum[K <: K3, G[_]]:
  type A[_[_]]
  val key: K[A]
  val value: A[G]
  final def toDPair: DPair[K, FlipAp[G]] = key -> (value : FlipAp[G][A])

object VSum:
  given[K <: K3, V[f[_]], G[_]] as Conversion[(K[V], V[G]), VSum[K, G]] = 
    tup => new: 
      type A[f[_]] = V[f]
      val (key, value) = tup  

object Vessel:
  opaque type Vessel[K <: K3, G[_]] <: DGet[K, FlipAp[G]] = MonoidalDMap[K, FlipAp[G]]
  // hs:fromListV
  def apply[K <: K3, G[_]](elems: VSum[K, G]*)(using Has[View, K]): Vessel[K, G] = 
    MonoidalDMap((
      for vsum <- elems if vsum.key.constraintsFor.nullV(vsum.value)
      yield vsum.toDPair
    ): _*)
    
  private def [K <: K3, G[_]] (map: MonoidalDMap[K, FlipAp[G]]) filterNullFlipAps (using Has[View, K]): MonoidalDMap[K, FlipAp[G]] =
    MonoidalDMap((
      for e <- map.toList
          k <- e.key.constraintsFor.collapseNullV(e.value)
      yield (e.key -> (k: FlipAp[G][e.A]): DPair[K, FlipAp[G]])
    ): _*)     

  extension on [K <: K3, G[_]](self: Vessel[K, G]):
    def set(vsum: VSum[K, G])(using Eq[K], Has0[Semigroup, FlipAp[G], K], Has[View, K]): Vessel[K, G] =
      self ++ Vessel(vsum)
    // hs:Semigroup (but right-biased of course)
    def ++(that: Vessel[K, G])(using Eq[K], Has0[Semigroup, FlipAp[G], K], Has[View, K]): Vessel[K, G] =
      filterNullFlipAps(MonoidalDMap.ops.++(self)(that))
    def toList: List[VSum[K, G]] =
      self.toList.map(e => e.key -> e.value)

  given[K <: K3: Eq](using Has[View, K]) as View[[f[_]] =>> Vessel[K, f]]:
    def nullV[F[_]](container: Vessel[K, F]) = container.toList.isEmpty 

@main def checkVessel() = 
  val monoidalDMap =
    MonoidalDMap(ExampleGADT.S -> ExampleValue[LS](LS(List(Option("str"))))) ++
      MonoidalDMap(ExampleGADT.I -> ExampleValue[LI](LI(List(Option(1)))))

  val vessel =
    Vessel(ExampleGADT.S -> LS[Option](List(Option("str")))) ++
    Vessel(ExampleGADT.I -> LI[Option](List(Option(1))))

  println(monoidalDMap: MonoidalDMap[ExampleGADT, ExampleValue])
  println(vessel: Vessel[ExampleGADT, Option])