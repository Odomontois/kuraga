package playground.chat.polyfunc

import scala.quoted.*

class Kenk[-T, +U](f: T => U)

// object Kenk:
//     transparent inline def of(f: Any): Kenk[Nothing, Any] =
//         ${ fmacro('f) }

//     private def fmacro(f: Expr[Any])(using q: Quotes): Expr[Kenk[Nothing, Any]] = 
//         import q.reflect.*
//         f.asTerm match 
//             case Lambda(defs, body) =>
//                 val types = defs.map(_.tpt.tpe)
//                 val tupleType = '[(...$types)]
//             case _ => report.errorAndAbort("Expected a lambda expression")

