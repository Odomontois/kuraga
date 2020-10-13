package playground.chat
import scala.quoted._

def printType[T : Type](using QuoteContext): Expr[String] = 
    val t = summon[Type[T]].unseal.show
    Expr(t)

inline def whatType[T](x : T): String = ${ printType[T] }