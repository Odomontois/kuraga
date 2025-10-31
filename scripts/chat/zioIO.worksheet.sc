//> using scala "3.8.0-RC1-bin-20250822-658c8bd-NIGHTLY"
//> using dep org.typelevel::cats-effect:3.6.3
//> using dep dev.zio::zio:2.1.21

import cats.effect.IO
import zio.{ZIO, RIO}
import zio.Unsafe

def fctx[A, B](f: A ?=> B)(a: A): B = f(using a)

// def toIO[R, A](t: zio.RIO[R, A]): zio.URIO[R, IO[A]] =
//   ZIO.runtime.flatMap: runtime =>
//     ZIO.succeedUnsafe:
//       fctx:
//         IO.fromFutureCancelable:
//           IO:
//             val fut = runtime.unsafe.runToFuture(t)
//             (fut, IO(fut.cancel()))

val z = ZIO.unit.*>(ZIO.unit) 
