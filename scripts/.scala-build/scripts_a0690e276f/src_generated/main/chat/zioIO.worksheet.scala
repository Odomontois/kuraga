package chat


final class zioIO$u002Eworksheet$_ {
def args = zioIO$u002Eworksheet_sc.args$
def scriptPath = """chat/zioIO.worksheet.sc"""
/*<script>*/
//> using dep dev.zio::zio:2.1.21
//> using dep org.typelevel::cats-effect:3.6.3

import cats.effect.IO
import zio.{ZIO, RIO}
import zio.Unsafe

def fctx[A, B](f: A ?=> B)(a: A): B = f(using a)

def toIO[R, A](t: zio.RIO[R, A]): zio.URIO[R, IO[A]] =
  ZIO.runtime.flatMap: runtime =>
    ZIO.succeedUnsafe:
      fctx:
        IO.fromFutureCancelable:
          IO:
            val fut = runtime.unsafe.runToFuture(t)
            (fut, IO(fut.cancel()))

/*</script>*/ /*<generated>*//*</generated>*/
}

object zioIO$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new zioIO$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export zioIO$u002Eworksheet_sc.script as `zioIO.worksheet`

