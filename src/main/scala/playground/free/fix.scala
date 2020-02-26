package playground
package free.fix
import scala.annotation.tailrec

type |@[F[+_], G[+_]] = [a] =>> F[a] | G[a]

final case class Cat[+R](name: String, fur: String, rest: R)
object Cat
    def of[R, F[+_]](name: String, fur: String, rest: Fix[F]): Fix[F |@ Cat] = Fix(new Cat(name, fur, rest))

final case class Dog[+R](name: String, size: Long, rest: R)
object Dog
    def of[R, F[+_]](name: String, size: Long, rest: Fix[F]): Fix[F |@ Dog] = Fix(new Dog(name, size, rest))

case object End{
    type f[+a] = End.type
    def apply() = Fix[f](End)
}


opaque type Fix[+F[+_]] = AnyRef

object Fix
    extension on [F[+_]](ff: Fix[F])     
        def value: F[Fix[F]] = ff.asInstanceOf
    def apply[F[+_]](fff: F[Fix[F]]): Fix[F] = fff.asInstanceOf


object DropRed
    @tailrec def dropRedCats[F[+a] >: Cat[a]](cats: Fix[F]): Fix[F] = 
        cats.value match 
            case Cat(_, "red", rest) => dropRedCats(rest)
            case _ => cats

    type CatDogVector = Vector[Either[Cat[Unit], Dog[Unit]]]
    type CatOrDogs[+a] =  Cat[a] | Dog[a] | End.type

    def (catDogs: Fix[CatOrDogs]) toVector : CatDogVector  = 
        @tailrec def go(acc: CatDogVector, catDogs: Fix[CatOrDogs]) : CatDogVector = catDogs.value match
            case Cat(name, fur, rest) =>  go(acc :+ Left(Cat(name, fur, ())), rest)
            case Dog(name, size, rest) => go(acc :+ Right(Dog(name, size, ())), rest)
            case End => acc
        
        go(Vector(), catDogs)

    val x = 
        Cat.of("lilly" , "red"  , 
        Cat.of("anya"  , "red"  , 
        Cat.of("boris" , "black", 
        Dog.of("mashka", 3      ,
        Cat.of("manya" , "red"  ,     
        End())))))

        
    val y = null.asInstanceOf[Fix[Cat |@ Dog |@ End.f]].value
    def main(args: Array[String]) = 
        println(x.toVector)
        println(dropRedCats(x).toVector)

