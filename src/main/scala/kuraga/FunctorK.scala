package kuraga 

type Rep[-U[f[_]], A] = [F[_]] => U[F] => F[A]


trait FunctorK[U[f[_]]]:
  extension [F[_], G[_]] (obj: U[F]) 
    def mapK (f: [A] => F[A] => G[A]): U[G]

trait ApplyK[U[f[_]]] extends FunctorK[U]:
  extension [F[_], G[_], H[_]] (left: U[F])
    def map2K(right: U[G])(f: [A] => (F[A], G[A]) => H[A]): U[H]

trait PureK[U[f[_]]] extends FunctorK[U]:
  def pureK[F[_]](gen: [A] => () => F[A]): U[F]

trait ApplicativeK[U[f[_]]] extends ApplyK[U] with PureK[U]:  

  extension [F[_], G[_]] (obj: U[F])
    override def mapK(f: [A] => F[A] => G[A]): U[G] = 
      obj.map2K(
        pureK[[A] =>> Unit]([A] => () => ()))(
          [A] => (x: F[A], y: Unit) => f(x))

trait RepresentableK[U[f[_]]] extends ApplicativeK[U]:
  def tabulate[F[_]](gain: [A] => Rep[U, A] => F[A]): U[F]

  override def pureK[F[_]](gen: [A] => () => F[A]): U[F] = 
    tabulate([A] => (rep: Rep[U, A]) => gen[A]())

  extension [F[_], G[_], H[_]](left: U[F])
    def map2K(right: U[G])(f: [A] => (F[A], G[A]) => H[A]): U[H] = 
      tabulate([A] => (rep: Rep[U, A]) => f[A](rep(left) , rep(right)))

trait TraverseK[U[f[_]]] extends FunctorK[U]:
  extension[F[_], G[+_], H[_]](uf: U[F])
    def traverseK(f: [A] => F[A] => G[H[A]])(
      using Applicative[G]): G[U[H]]

  extension[F[+_], G[_]](uf: U[[A] =>> F[G[A]]])
    def sequenceK(using Applicative[F]): F[U[G]] = uf.traverseK([A] => (a : F[G[A]]) => a)

trait Craft[U[f[_]]] extends RepresentableK[U] with TraverseK[U]:
  def craft[F[+_]: Applicative, G[_]](gain: [A] => Rep[U, A] => F[G[A]]): F[U[G]]

  def tabulate[F[_]](gain: [A] => Rep[U, A] => F[A]): U[F] = craft[Identity, F](gain)

  extension[F[_], G[+_], H[_]](uf: U[F])
    def traverseK(f: [A] => F[A] => G[H[A]])(using Applicative[G]): G[U[H]] = 
      craft[G, H]([A] => (frep: Rep[U, A]) => f(frep(uf)))

import scala.compiletime.*
import scala.deriving.*
package hkd{
  inline def provision[P](using p: Mirror.ProductOf[P]): P = 
      p.fromProduct(summonAll[p.MirroredElemTypes])

  type Provided[Data[f[_]], TC[_]] = Data[TC]

  type Name[A] = String

  type Names[Data[f[_]]] = Data[Name]

  inline def names[Data[f[_]]](using p: Mirror.ProductOf[Data[Name]]): Data[Name] = 
      p.fromProduct(constValueTuple[p.MirroredElemLabels])

}