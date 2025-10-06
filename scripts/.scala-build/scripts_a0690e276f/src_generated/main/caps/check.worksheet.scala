package caps


final class check$u002Eworksheet$_ {
def args = check$u002Eworksheet_sc.args$
def scriptPath = """caps/check.worksheet.sc"""
/*<script>*/
//> using scala 3.7.3
//> using options "-language:experimental.captureChecking"

import scala.caps.{Capability, cap}

class Foo extends Capability
class Bar(u: Any)

def xx(f: Foo -> Unit): Unit = f(Foo())

def zz() = 
  val foo: Foo^{cap} = Foo()
  val bar: Bar^{foo} = Bar(foo)
  println("lol")


1 + 2

/*</script>*/ /*<generated>*//*</generated>*/
}

object check$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new check$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export check$u002Eworksheet_sc.script as `check.worksheet`

