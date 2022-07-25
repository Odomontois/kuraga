package dailytips.typeName

import quoted.{Expr, Quotes, Type}

inline def typeName[A](inline a: A): String =
  ${ typeNameMacro[A] }

private def typeNameMacro[A: Type](using Quotes) =
  Expr(Type.show[A])
