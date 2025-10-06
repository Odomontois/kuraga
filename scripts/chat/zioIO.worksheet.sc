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
