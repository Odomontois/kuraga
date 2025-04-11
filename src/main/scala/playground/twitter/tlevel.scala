package playground.twitter

import scala.quoted.{Expr, Quotes, Type}

object CI {
    opaque type CI[p <: String] <: String = String

    def unsafeConvert[s <: String, p <: String]: Conversion[s, CI[p]] = new Conversion[s, CI[p]] {
        def apply(s: s): CI[p] = s
    }

    inline given [s <: String & Singleton, p <: String]: Conversion[s, CI[p]] = ${ convertImpl[s, p] }

    private def convertImpl[s <: String: Type, p <: String: Type](using q: Quotes): Expr[Conversion[s, CI[p]]] = {
        import q.reflect.*

        def getConstant[s <: String: Type]: String =
            TypeRepr.of[s] match
                case ConstantType(StringConstant(str)) => str
                case _                                 => report.errorAndAbort(s"Constant string expected, got ${Type.show[s]}")

        val s = getConstant[s]
        val p = getConstant[p]

        if s.toLowerCase() == p.toLowerCase() then '{ CI.unsafeConvert[s, p] }
        else report.errorAndAbort(s"String literal $s does not match $p")
    }
}

export CI.CI
