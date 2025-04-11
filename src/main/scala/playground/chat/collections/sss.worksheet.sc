final abstract class TableName

trait Ops[Lang[r[_]]]  {
    type Expr[R[_], A] = Lang[R] ?=> R[A]
    def lang[R[_]](using lang: Lang[R]): Lang[R] = lang
}
//alg#1
trait TableNames[A[_]] {
    def tableName(name: String): A[TableName]
}
object TableNames extends Ops[TableNames] {
    def tableName[R[_]](name: String): Expr[R, TableName] = lang.tableName(name)
}

//alg#2
trait Conditions[R[_]] {
    def and(a: R[Boolean], b: R[Boolean]): R[Boolean]
    def not(a: R[Boolean]): R[Boolean]
    def attributeExists(s: String): R[Boolean]
    def when[A](a: R[Boolean], b: R[A]): R[A]
}

object Conditions extends Ops[Conditions] {
    def and[R[_]](a: Expr[R, Boolean], b: Expr[R, Boolean]): Expr[R, Boolean] = lang.and(a, b)
    def not[R[_]](a: Expr[R, Boolean]): Expr[R, Boolean]                      = lang.not(a)
    def attributeExists[R[_]](s: String): Expr[R, Boolean]                    = lang.attributeExists(s)
    def when[R[_], A](a: Expr[R, Boolean], b: Expr[R, A]): Expr[R, A]         = lang.when(a, b)
}

type PutItemLang[R[_], A] = (TableNames[R], Conditions[R]) ?=> R[A]

def textExpr[R[_]]: PutItemLang[R, TableName] = {
    import Conditions._
    import TableNames._
    when(
      and(
        attributeExists("lol2"),
        not(attributeExists("lol")) // correct
        // not(tableName("lol")) // incorrect => should not compile
      ),
      tableName("table"),
    )
}

import cats.Show

class Kek
class Lol

object KekOps:
    extension [A](a: Kek) def foo = "kek"
given KekOps.type = KekOps

object LolOps:
    extension [A](a: Lol) def foo = "lol"
given LolOps.type = LolOps

Kek().foo
Lol().foo
