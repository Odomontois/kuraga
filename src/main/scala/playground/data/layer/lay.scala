package playground
package data.layer
import scala.annotation.tailrec
import cats.Show
import cats.syntax.show.given
import cats.instances.vector.given
import cats.instances.either.given


trait Layer[-P[-_, _]]
    def peel[R](cont: P[Layer[P], R]) : R

trait Data[-P[-_, _]]
    def read[R](cont: P[Any, R]): R

trait Cat[-I, +O]
    def cat(name: String, fur: String, rest: I): O

object Cat
    def apply[P[-i, o] <: Cat[i, o]](name: String, fur: String, rest: Layer[P]): Layer[P] = 
        new {
            def peel[R](cont: P[Layer[P], R]): R = cont.cat(name, fur, rest)
        }  

    def of(name: String, fur: String): Data[Cat] = 
        new {
            def read[R](cont: Cat[Any, R]) = cont.cat(name, fur, ())
        }
        
    
    given Show[Data[Cat]] = _.read((name, fur, _) => s"Cat{name=$name, fur=$fur}")

trait Dog[-I, +O]
    def dog(name: String, size: Long, rest: I): O

object Dog
    def apply[P[-i, o] <: Dog[i, o]](name: String, size: Long, rest: Layer[P]): Layer[P] = 
        new {
            def peel[R](cont: P[Layer[P], R]): R = cont.dog(name, size, rest)
        }

    def of(name: String, size: Long): Data[Dog] = 
        new {
            def read[R](cont: Dog[Any, R]) = cont.dog(name, size, ())
        }

    given Show[Data[Dog]] = _.read((name, size, _) => s"Dog{name=$name, size=$size}")


trait End[+O]
    def end: O

object End extends Layer[[-i, o] =>> End[o]]
    def peel[R](cont: End[R]): R = cont.end

object DropRed
    def dropRedCats(cats: Layer[CatOrDogs]): Layer[CatOrDogs] = 
        val (cont, res) = cats.peel[(Boolean, Layer[CatOrDogs])](new {
            def cat(name: String, fur: String, rest: Layer[CatOrDogs]) = 
                if fur == "red" then (true,  rest)  else (false, cats)
            def dog(name: String, size: Long, rest: Layer[CatOrDogs]) = (false, cats)
            def end = (false, cats)
        })
        if cont then dropRedCats(res) else res

    type CatDogVector = Vector[Either[Data[Cat], Data[Dog]]]
    trait CatOrDogs[-i, +o] extends  Cat[i, o] with Dog[i, o] with End[o]

    def (catDogs: Layer[CatOrDogs]) toVector : CatDogVector  = 
        var res: CatDogVector = Vector()
        @tailrec def go(catDogs: Layer[CatOrDogs]) : CatDogVector =
            val (continue, next) = catDogs.peel[(Boolean, Layer[CatOrDogs])](new {
                def cat(name: String, fur: String, rest: Layer[CatOrDogs]) = 
                    res = res :+ Left(Cat.of(name, fur))
                    (true, rest)
                def dog(name: String, size: Long, rest: Layer[CatOrDogs]) = 
                    res = res :+ Right(Dog.of(name, size)) 
                    (true, rest)               
                def end = (false, catDogs)
            })
            if continue then go(next) else res            
        
        go(catDogs)

    val x = Cat("lilly" , "red"  , 
            Cat("anya"  , "red"  , 
            Cat("boris" , "black", 
            Dog("mashka", 3      ,
            Cat("manya" , "red"  ,
            End)))))

        
    def main(args: Array[String]) = 
        println(x.toVector.show)
        println(dropRedCats(x).toVector.show)
