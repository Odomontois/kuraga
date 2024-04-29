package kuraga.typenum

import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type

class Search[+Ts <: Tuple]
object Tagged:
    transparent inline def findAll[T[+_]]: Search[Tuple] = ${ Tagged.findAllMacro[T] }

    def findAllMacro[T[+_]: Type](using Quotes): Expr[Search[Tuple]] = Tagged[T].findAllMacro

end Tagged

class Tagged[T[+_]: Type](using q: Quotes):
    import q.reflect.*
    def findAllMacro: Expr[Search[Tuple]] =
        import q.reflect.*
        val t = TypeRepr.of[T[Any]]
        Implicits.search(t) match {
            case suc: ImplicitSearchSuccess => build(suc.tree)
            case ambig: AmbiguousImplicits =>
                val explFld = ambig.getClass().getDeclaredField("expl").nn
                explFld.setAccessible(true)
                val fn = explFld.get(ambig).asInstanceOf[() => String]
                // report.info(fn())
                val fds = fn.getClass().getDeclaredFields()
                report.info(fn.getClass().getName().nn)
                report.errorAndAbort(ambig.toString())
            case fail: ImplicitSearchFailure =>
                report.errorAndAbort(fail.explanation)
        }
    end findAllMacro

    private def build(trees: Term*): Expr[Search[Tuple]] = '{
        new Search[EmptyTuple]:
            override def toString = ${ Expr(trees.map(_.show).mkString("[", ", ", "]")) }
    }
end Tagged
