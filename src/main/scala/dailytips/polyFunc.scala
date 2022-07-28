package dailytips.depfunc

import java.time.ZonedDateTime
import reflect.TypeTest

type Author = String

case class Comment(
    author: Author,
    text: Vector[String],
    dateTime: ZonedDateTime
)

type CommentFields = "author" | "text" | "dateTime"

type CommentF[name <: CommentFields] = name match
  case "author"   => Author
  case "text"     => Vector[String]
  case "dateTime" => ZonedDateTime

type CommentDep = (field: CommentFields) => CommentF[field.type]

def commentToF(comment: Comment): CommentDep =
  case _: "author"   => comment.author
  case _: "text"     => comment.text
  case _: "dateTime" => comment.dateTime

def commentFromF(f: CommentDep) = Comment(
  author = f("author"),
  text = f("text"),
  dateTime = f("dateTime")
)

@main def foo() =
  val comment = Comment("odo", Vector("hello", "world"), ZonedDateTime.now())
  val f       = commentToF(comment)

  summon[TypeTest["yyy", "xxx"]]

  println(f("author"))
  println(f("text"))
  println(f("dateTime"))

  println(commentFromF(f))


sealed abstract class :<:[A, B]:
  type T >: A <: B

object <:< :
  def apply[A <: B, B] = new { type T = A }

enum Eqs[X, Y]:
  case Yes(lt: X <:< Y, gt: Y <:< X)
  case No()

//def compare(x: String, y: String): Eqs[x.type, y.type] =
//  (x : x.type) match
//    case yy: y.type =>
//      yy match
//        case xx : x.type =>
//          xx : x.type & y.type
//          ???

class Change[fields, Field[_ <: fields]](val name: fields)(val value: Field[name.type]):
  def apply(f: (x: fields) => Field[x.type])(x: fields)(using tt: TypeTest[x.type, name.type]): Field[x.type] =
    tt.unapply(x) match
      case Some(xn) => ???
      case None     => f(x)

type CommentChanges = List[Change[CommentFields, CommentF]]
