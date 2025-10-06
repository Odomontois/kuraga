package chat


final class matching$u002Eworksheet$_ {
def args = matching$u002Eworksheet_sc.args$
def scriptPath = """chat/matching.worksheet.sc"""
/*<script>*/
import scala.NamedTuple.AnyNamedTuple as ANT
import scala.reflect.TypeTest

case class Named[+Name <: String, +V <: ANT](name: Name, v: V) 

class Companion[Name <: String , V <: ANT](using name: ValueOf[Name]):
  opaque type Err <: Named[Name, V] = Named[Name, V]

  def apply(v: V): Err = Named(name.value, v)

  given [N1 <: String, V1 <: ANT](using TypeTest[N1, Name], TypeTest[V1, V]): TypeTest[Named[N1, V1], Err] with 
    def unapply(x: Named[N1, V1]): Option[x.type & Err] = 
        x match
            case Named(name: Name, value: V) => Some(x.asInstanceOf[x.type & Err])      
            case _ => None

  def unapply(x: Err): V = x.v
    

val Err1 = new Companion["Err1", (msg: String, code: Int)]
val Err2 = new Companion["Err2", (msg: String, code: Int)]
import Err1.Err as Err1
import Err2.Err as Err2
// ok
def handle(res: Err1 | Err2): Unit =
  res match
    case Named("Err1", (msg, code)) => println(s"ERROR1: $msg $code")
    case Named("Err2", (msg, code)) => println(s"ERROR2: $msg $code")

handle(Err2("test1", 42))

// not ok
def handle2(res: Err1 | Err2): Unit =
  res match
    case Err1(msg, code) => println(s"ERROR1: $msg $code")
    case Err2(msg, code) => println(s"ERROR2: $msg $code")

def handle3(res: Err1 | Err2): Unit =
  res match
    case _: Err1 => println(s"ERROR1")
    case _: Err2 => println(s"ERROR2")

handle2(
  Err2("test2", 42)
) // хочу ERROR2: test2 42, получаю ERROR1: test2 42
// (summon[TypeTest[(str: Int), (str: String)]]
//     .unapply((str = 1)) : Option[(str: String)])
//     .getOrElse((str = "hello"))
//     .str
    
summon[TypeTest[Err1 | Err2, Err1]]
summon[TypeTest[Err1 | Err2, Err2]]
/*</script>*/ /*<generated>*//*</generated>*/
}

object matching$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new matching$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export matching$u002Eworksheet_sc.script as `matching.worksheet`

