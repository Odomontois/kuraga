package playground
import scala.quoted.*
object Kek:
    def tag[A: Type](a: Expr[A])(using Quotes): Expr[Unit] = 
        val x = summon[Type[A]].toString
        '{println(${Expr(x)})}   
    

    inline def printType[A](x: A) : Unit = 
        ${ tag('{x}) }

