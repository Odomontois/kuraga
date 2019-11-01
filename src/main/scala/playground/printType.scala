package playground
import scala.quoted._
def tag[A: Type](a: Expr[A])(given QuoteContext): Expr[Unit] = 
    val x = summon[Type[A]].show
    '{println(${Expr(x)})}   
  

inline def printType[A](x: A) : Unit = 
    ${ tag('{x}) }

