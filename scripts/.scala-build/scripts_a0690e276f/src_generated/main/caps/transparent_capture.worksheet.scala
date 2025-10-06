package caps


final class transparent_capture$u002Eworksheet$_ {
def args = transparent_capture$u002Eworksheet_sc.args$
def scriptPath = """caps/transparent_capture.worksheet.sc"""
/*<script>*/
//> using scala 3.7.4-RC1
//> using options "-language:experimental.captureChecking"







/*</script>*/ /*<generated>*//*</generated>*/
}

object transparent_capture$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new transparent_capture$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export transparent_capture$u002Eworksheet_sc.script as `transparent_capture.worksheet`

