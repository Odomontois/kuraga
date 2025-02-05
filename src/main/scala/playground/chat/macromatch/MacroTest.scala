package playground.chat.macromatch

object MacroTest:
  import scala.quoted.*

  inline def checkMatch(inline vargs: Any*): Seq[Any] = ${ checkMatchImpl('{ vargs }) }

  private def checkMatchImpl(vargs: Expr[Seq[Any]])(using Quotes): Expr[Seq[Any]] =
    vargs match
      case Varargs(args) =>
        val args2 = args.map {
          case '{ (${ opt }: Option[a]).exists($v) } =>
            Expr(s"[+] exists with ${v.show} , [${Type.show[a]}]")
          case other =>
            Expr(s"[-] ${other.show}")
        }
        Varargs[Any](args2)
end MacroTest