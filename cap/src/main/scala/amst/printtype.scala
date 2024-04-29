package amst

import scala.quoted.Type
import scala.quoted.Expr
import scala.quoted.Quotes


def printTypeImpl[A: Type](a: Expr[A])(using q: Quotes): Expr[String] =
  Expr(q.reflect.TypeRepr.of[A].simplified.show)

inline def printType[A](x: A): String = ${ printTypeImpl('x) }