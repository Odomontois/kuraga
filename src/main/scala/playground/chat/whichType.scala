package playground.chat
import scala.quoted._


object Kek: 
    def printType[T : Type](using Quotes): Expr[String] = 
        val t = summon[Type[T]].toString
        Expr(t)

    inline def whatType[T](x : T): String = ${ printType[T] }   