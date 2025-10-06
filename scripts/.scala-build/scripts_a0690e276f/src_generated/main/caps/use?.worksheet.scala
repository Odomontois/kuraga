package caps


final class use$qmark$u002Eworksheet$_ {
def args = use$qmark$u002Eworksheet_sc.args$
def scriptPath = """caps/use?.worksheet.sc"""
/*<script>*/
//> use scala 3.7.4-RC1
//> use options "language.experimental.captureChecking"

1 + 2

/*</script>*/ /*<generated>*//*</generated>*/
}

object use$qmark$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new use$qmark$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export use$qmark$u002Eworksheet_sc.script as `use?.worksheet`

