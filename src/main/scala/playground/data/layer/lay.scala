package playground
package data.layer
import scala.annotation.tailrec
import cats.Show
import cats.syntax.show.given
import cats.instances.vector.given
import cats.instances.either.given
import tofu.optics.Contains

trait Layer[-P[-_, _]]
    def peel[R](cont: P[Layer[P], R]) : R

trait Data[-P[-_, _]]
    def read[R](cont: P[Any, R]): R

trait Cat[-I, +O]
    def cat(name: String, fur: String, rest: I): O

object Cat
    def apply[P[-i, o] <: Cat[i, o]](name: String, fur: String, rest: Layer[P]): Layer[P] = 
        new { def peel[R](cont: P[Layer[P], R]): R = cont.cat(name, fur, rest) }  

    def of(name: String, fur: String): Data[Cat] =
        new { def read[R](cont: Cat[Any, R]) = cont.cat(name, fur, ()) }        
    
    given Show[Data[Cat]] = _.read((name, fur, _) => s"Cat{name=$name, fur=$fur}")

trait Dog[-I, +O]
    def dog(name: String, size: Long, rest: I): O

object Dog
    def apply[P[-i, o] <: Dog[i, o]](name: String, size: Long, rest: Layer[P]): Layer[P] = 
        new { def peel[R](cont: P[Layer[P], R]): R = cont.dog(name, size, rest) }

    def of(name: String, size: Long): Data[Dog] = 
        new { def read[R](cont: Dog[Any, R]) = cont.dog(name, size, ()) }

    given Show[Data[Dog]] = _.read((name, size, _) => s"Dog{name=$name, size=$size}")


trait End[+O]
    def end: O

object End extends Layer[[-i, o] =>> End[o]]
    def peel[R](cont: End[R]): R = cont.end

trait Constant[P[-_, _]]
    def const[B](b : B) : P[Any, B]

trait Lens2[P[_, _], Q[_, _]]
    def get[A, B](p: P[A, B]) : Q[A, B]
    def set[A, B](p: P[A, B])(q: Q[A, B]): P[A, B]

object DropRed
    def dropRedCats[P[-i, o]](cats: Layer[P])(given P: Constant[P], pc: Lens2[P, Cat]): Layer[P] = 
        val (cont, res) =  cats.peel( 
            pc.set(P.const((false, cats)))((name, fur, rest) => if fur == "red" then (true,  rest)  else (false, cats)))     

        if cont then dropRedCats(res) else res

    type CatDogVector = Vector[Either[Data[Cat], Data[Dog]]]
    trait CatOrDogs[-i, +o] extends  Cat[i, o] with Dog[i, o] with End[o]

    given Constant[CatOrDogs]
        def const[B](b: B) = new {
            def cat(name: String, fur: String, rest: Any) = b
            def dog(name: String, size: Long, rest: Any) = b
            def end = b
        }
    
    given Lens2[CatOrDogs, Cat]
       def get[A, B](p: CatOrDogs[A, B]): Cat[A, B] = p
       def set[A, B](p: CatOrDogs[A, B])(q: Cat[A, B]): CatOrDogs[A, B] = new {
           export q.cat
           export p.{dog, end}
       }


    def (catDogs: Layer[CatOrDogs]) toCatDogVector: CatDogVector  = 
        @tailrec def go(catDogs: Layer[CatOrDogs], acc: CatDogVector = Vector()) : CatDogVector =
            val (continue, next, res) = catDogs.peel[(Boolean, Layer[CatOrDogs], CatDogVector)](new {
                def cat(name: String, fur: String, rest: Layer[CatOrDogs]) =                     
                    (true, rest, acc :+ Left(Cat.of(name, fur)))
                def dog(name: String, size: Long, rest: Layer[CatOrDogs]) = 
                    (true, rest, acc :+ Right(Dog.of(name, size)))               
                def end = (false, catDogs, acc)
            })
            if continue then go(next, res) else res            
        
        go(catDogs)

    val x = 
        Cat("lilly" , "red"  , 
        Cat("anya"  , "red"  , 
        Cat("boris" , "black", 
        Dog("mashka", 3      ,
        Cat("manya" , "red"  ,
        End)))))

        
    def main(args: Array[String]) = 
        println(x.toCatDogVector.show)
        println(dropRedCats[CatOrDogs](x).toCatDogVector.show)
