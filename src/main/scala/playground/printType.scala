package playground
import scala.quoted._
def tag[A: Type](a: Expr[A])(using QuoteContext): Expr[Unit] = 
    val x = summon[Type[A]].show
    '{println(${Expr(x)})}   
  

inline def printType[A](x: A) : Unit = 
    ${ tag('{x}) }

