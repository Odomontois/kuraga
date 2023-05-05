package playground.ssssss

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.language.dynamics

enum Token:
    case V(name: String)
    case Num(value: Int)
    case Or
    case And

sealed trait Expression:
    def assignments: Seq[Assignment]
    def and(other: Assignment) = Conjunction((assignments :+ other)*)

case class Assignment(name: String, values: Int*) extends Expression:
    def |(value: Int) = Assignment(name, (values :+ value)*)
    def assignments   = Vector(this)

case class Conjunction(assignments: Assignment*) extends Expression

inline def assignment(inline self: Var) = ${ assignmentImpl('self) }

def assignmentImpl(self: Expr[Var])(using q: Quotes): Expr[Assignment] =
    import q.reflect.*
    val name = self.asTerm.underlyingArgument match
        case Ident(name)     => name
        case Select(_, name) => name
        case ts              => report.errorAndAbort(s"tokens: $ts")

    '{ Assignment(${ Expr(name) }) }

class Var:
    inline def ~(value: Int) = assignment(this) | value
