//> using dep org.typelevel::cats-mtl:1.6.0
//> using dep dev.optics::monocle-macro:3.3.0
//> using options -Xkind-projector:underscores
//> using scala 3.7.2

import cats.*
import cats.data.Reader
import cats.mtl.*
import monocle.macros.GenLens
import monocle.*
import cats.syntax.all.*
import cats.mtl.syntax.all.*
import cats.instances.all.*


def localFromLens[F[_]: Loc[S] as loc, S, A](l: Lens[S, A]): Local[F, A] = new:
    given applicative: Applicative[F] = loc.applicative
    def ask[X >: A]                   = loc.ask.map(l.get)
    def local[X](fa: F[X])(f: A => A) = loc.local(fa)(l.modify(f))
end localFromLens
val u : Int <:< Int = <:<.refl


inline def focusOn[F[_], S, A, R](using loc: Local[F, S])(inline path: Focus.KeywordContext ?=> S => A)(
    body: Local[F, A] ?=> R
): R =
    inline GenLens[S].apply(path) match
        case l: Lens[S, A] => body(using localFromLens(l)) 

def mumu[F[_]: Loc[String], X]: F[String] = ask.local("Hi, " + (_: String))

case class Ctx(firstName: String, lastName: String)

type Loc[Ctx] = [F[_]] =>> Local[F, Ctx]
def ask[F[_], A](using a: Ask[F, A]): F[A] = a.ask

def foo[F[_]: {Monad, Loc[Ctx]}]: F[String] = (
  focusOn(_.firstName):
    mumu
  ,
  focusOn(_.lastName):
    mumu
  ).mapN(_ + "\n" + _)


foo[Reader[Ctx, _]].run(Ctx("Geoffs", "Lindsay"))
