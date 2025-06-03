// package playground.chat.tables.tata
import scala.language.experimental

import NamedTuple.{AnyNamedTuple, Names, DropNames, Concat, NamedTuple}
import scala.annotation.experimental
import java.util.UUID
import scala.compiletime.ops.int.{+}
import scala.compiletime.ops.any
import scala.compiletime.ops.string.{+ as ++}
import scala.compiletime.{constValue, error, constValueTuple, summonInline, summonFrom}
import java.time.ZonedDateTime
import scala.deriving.Mirror

type IndexOf[X, T <: Tuple, acc <: Int] <: Int = T match
    case X *: xs    => acc
    case _ *: xs    => IndexOf[X, xs, acc + 1]
    case EmptyTuple => -1

type TypeOf[T <: AnyNamedTuple, name] = Tuple.Elem[DropNames[T], IndexOf[name, Names[T], 0]]

enum Constraint:
    case EQ, NE, GT, LT

object Constraint:
    type ToString[T <: Constraint] <: String = T match
        case EQ.type => "=="
        case NE.type => "<>"
        case GT.type => ">"
        case LT.type => "<"
import Constraint.*

def col(using table: Table[?, ?]): Unit = ()

def cantRun(using absurd: Nothing): Nothing = absurd

final class Columns[acc <: Tuple, t <: AnyTable](val t: t) {
    type AddField[name] = Columns[name *: acc, t]
    type Choose         = NamedTuple[Names[t.AllFields], Tuple.Map[t.AllFieldNames, AddField]]
    inline def $(using Nothing): Choose = cantRun
}

final class Filters[acc <: Tuple, t <: AnyTable](table: t) extends Clause(table) {
    type SField[name] = AField[name, TypeOf[t.AllFields, name]]
    class AField[name, Type] {
        type Apply[cons <: Constraint, value <: Type] = Filters[(name, cons, value) *: acc, t]
        def ==(value: Type & Singleton) = apply(Constraint.EQ, value)
        def !=(value: Type & Singleton) = apply(Constraint.NE, value)
        def >(value: Type & Singleton)  = apply(Constraint.GT, value)
        def <(value: Type & Singleton)  = apply(Constraint.LT, value)
        def apply(
            cons: Constraint & Singleton,
            value: Type & Singleton,
        ): Filters[(name, cons.type, value.type) *: acc, t] = Filters(t)
    }
    type Choose = NamedTuple[Names[t.AllFields], Tuple.Map[t.AllFieldNames, SField]]
    inline def &&(using Nothing): Choose = cantRun

}

trait Clause[t <: AnyTable](val t: t) {
    type FieldName = Tuple.Union[Names[t.AllFields]]
}

final class SelectClause[fnames <: Tuple, t <: AnyTable](t: t) extends Clause(t) {
    def apply[name <: FieldName & Singleton](name: name): SelectClause[name.type *: fnames, t] = SelectClause(t)
    def where: WhereClause[fnames, EmptyTuple, t]                                              = WhereClause(t)
    inline def Where[acc <: Tuple](
        inline cols: Nothing ?=> (filters: Filters[EmptyTuple, t]) ?=> filters.Choose => Filters[acc, ?]
    ): WhereClause[fnames, acc, t] = WhereClause(t)
}

final class WhereClause[fnames <: Tuple, constrs <: Tuple, t <: AnyTable](val table: t) extends Clause(table) {
    def apply(
        name: FieldName & Singleton,
        cons: Constraint & Singleton,
        value: TypeOf[t.AllFields, name.type] & Singleton,
    ): WhereClause[fnames, (name.type, cons.type, value.type) *: constrs, t] = WhereClause(t)

    inline def show: String = {
        type ValueString[V] <: String = V match {
            case String => "'" ++ V ++ "'"
            case _      => any.ToString[V]
        }
        type ConstraintString[T]      = T match {
            case (name, constr, value) => name ++ " " ++ Constraint.ToString[constr & Constraint] ++ " " ++ ValueString[value]
        }
        val names = constValueTuple[fnames].toArray.mkString(", ")
        val constraints = constValueTuple[Tuple.Map[constrs, ConstraintString]]
        val constString = constraints.toArray.mkString(" AND ")
        val tableName   = table.tableName.toLowerCase()
        s"SELECT $names FROM $tableName WHERE $constString"
    }
}

trait AnyTable {
    type AllFields <: AnyNamedTuple
    type AllFieldNames = Names[AllFields]
    inline def tableName: String
}

trait Table[Fields <: AnyNamedTuple, PK] extends AnyTable with Product {
    type AllFields = Concat[(id: PK), Fields]

    class Condition[fn, cons <: Constraint, value]

    def select: SelectClause[EmptyTuple, this.type] = SelectClause(this)
    inline def tableName: String                    =
        summonFrom { case m: Mirror.ProductOf[this.type] =>
            tableNameOf(using m)
        }

    inline def tableNameOf[name <: String](using m: Mirror.ProductOf[this.type] { type MirroredLabel = name }): String =
        constValue[name]

    inline def Select[acc <: Tuple](
        inline cols: Nothing ?=> (columns: Columns[EmptyTuple, this.type]) ?=> columns.Choose => Columns[acc, ?]
    ): SelectClause[acc, this.type] =
        SelectClause(this)

    def get(p: PK): Unit = ()
}

case object Products extends Table[(description: String, price: Double, createdAt: ZonedDateTime), UUID]

Products.select("id")("description").where("price", GT, 23.0d)("description", NE, "")
Products.Select(_.id.$.description).Where(_.price.>(23.0).&&.description.!=(""))
Products.Select(_.id.$.description).Where(_.price.>(23.0).&&.description.!=("")).show
