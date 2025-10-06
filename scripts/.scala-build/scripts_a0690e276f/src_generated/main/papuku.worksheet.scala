

final class papuku$u002Eworksheet$_ {
def args = papuku$u002Eworksheet_sc.args$
def scriptPath = """papuku.worksheet.sc"""
/*<script>*/
def 🫱 () = 6
extension (u : Unit) infix def 🫲 (a: Any) = 7
val 👆 = 1

🫱( )*( )🫲
     👆
/*</script>*/ /*<generated>*//*</generated>*/
}

object papuku$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new papuku$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export papuku$u002Eworksheet_sc.script as `papuku.worksheet`

