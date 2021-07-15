package playground.hk

import kuraga.Applicative

// simple mixin with usable type aliases
trait Fiction[Q[_]]:
    type A[a] = Q[Fiction.Arg[a]]
    type S[e, a] = Q[Fiction.Sub[e, a]]
    type R[e, a] = Q[Fiction.Ret[e, a]]


object Fiction:
  // tags for type argument combination
  final class Arg[A]     
  final class Sub[E, A]
  final class Ret[E, A]

type Exposition[P[_], S[_, _], R[_, _], T] = T match 
    case Fiction.Arg[a]    => P[a]
    case Fiction.Sub[e, a] => S[e, a]
    case Fiction.Ret[e, a] => R[e, a]

//representation of trait modulo R
type Fabula[fiction[q[_]], P[_], S[_, _], E, A] = 
  [R[e, a]] => fiction[Exposition[P, S, R, *]] => R[E, A]

// natural transformation from representation to target R
type Retell[fiction[q[_]], P[_], S[_, _], R[_, _], F[_], G[_]] = 
    [E, A] => F[G[Fabula[fiction, P, S, E, A]]] => R[E, A]

trait FicBase[fiction[q[_]]]:
    type Exposed[P[_], S[_, _], R[_, _]] = fiction[Exposition[P, S, R, *]]

private type ~>[F[_], G[_]] = [A] => F[A] => G[A]
private type ~~>[F[_, _], G[_, _]] = [A, B] => F[A, B] => G[A, B]
private type Id[A] = A

// contravariant hk functor on arguments
trait MapArg[fiction[q[_]]] extends FicBase[fiction]:
    def mapArg[LP[_], RP[_], S[_, _], R[_, _]](
        f: LP ~> RP,
        fiction: Exposed[RP, S, R],
    ): Exposed[LP, S, R]

// contravariant hk functor on subprocesses
trait MapSub[fiction[q[_]]] extends FicBase[fiction]:
    def mapSub[P[_], LS[_, _], RS[_, _], R[_, _]](
        f: LS ~~> RS,
        fiction: Exposed[P, RS, R],
    ): Exposed[P, LS, R]

// covariant hk functor on returns
trait MapRet[fiction[q[_]]] extends FicBase[fiction]:
    def mapRet[P[_], S[_, _], LR[_, _], RR[_, _]](
        f: LR ~~> RR,
        fiction: Exposed[P, S, LR],
    ): Exposed[P, S, RR]

// representable instance of trifunctor (or biprofunctor idk)
trait Plot[fiction[q[_]]] extends MapRet[fiction]:
    def plot[P[_], S[_, _], R[_, _]](retell: Retell[fiction, P, S, R, Id, Id]): Exposed[P, S, R]

    def mapRet[P[_], S[_, _], LR[_, _], RR[_, _]](f: LR ~~> RR, fiction: Exposed[P, S, LR]): Exposed[P, S, RR] =
        plot([E, A] => (src: Fabula[fiction, P, S, E, A]) => f(src(fiction)))

//representable , also requiring bitraversing
trait Narrative[fiction[q[_]]] extends Plot[fiction] with MapArg[fiction] with MapSub[fiction]:
    import Narrative._
    def narrative[IP[_], IS[_, _], P[_], S[_, _], R[_, _], PF[_]: Applicative, SF[_]: Applicative](
        argScene: Scene1[IP, PF, P],
        subScene: Scene2[IS, SF, S],
        augRetell: Retell[fiction, P, S, R, PF, SF]
    ): Exposed[IP, IS, R]

    override def plot[P[_], S[_, _], R[_, _]](retell: Retell[fiction, P, S, R, Id, Id]): fiction[Exposition[P, S, R, *]] =
        narrative[P, S, P, S, R, Id, Id]([A] => (x : P[A]) => x, [A, B] => (x: S[A, B]) => x, retell)

    def bicontramap[IP[_], IS[_, _], P[_], S[_, _], R[_, _]](
        argF: IP ~> P,
        subF: IS ~~> S,
        fiction: Exposed[P, S, R]
    ): Exposed[IP, IS, R] = 
        narrative[IP, IS, P, S, R, Id, Id](argF, subF, [E, A] => (src: Fabula[fiction, P, S, E, A]) => src(fiction))

    override def mapArg[LP[_], RP[_], S[_, _], R[_, _]](
        f: LP ~> RP,
        fiction: Exposed[RP, S, R]
    ): Exposed[LP, S, R] = bicontramap[LP, S, RP, S, R](f, [A, B] => (x: S[A, B]) => x, fiction)

    override def mapSub[P[_], LS[_, _], RS[_, _], R[_, _]](
        f: LS ~~> RS,
        fiction: Exposed[P, RS, R]
    ): Exposed[P, LS, R] = bicontramap[P, LS, P, RS, R]([A] => (x: P[A]) => x, f, fiction)

object Narrative: 
  type Scene1[F[_], G[_], H[_]]       = [A] => F[A]  => G[H[A]]
  type Scene2[F[_, _], G[_], H[_, _]] = [A, B] => F[A, B] => G[H[A, B]]


case class AuthError(message: String)
case class Token(token: String)
trait Authorization[Q[_]] extends Fiction[Q]:
  def authorize[E, T](token: A[Token], action: S[E, T]): R[Either[E, AuthError], T]
  def authenticate(username: A[String], pass: A[String]): R[AuthError, Token]

object Authorization:
    import Narrative._
    given Narrative[Authorization] with
        def narrative[IP[_], IS[_, _], P[_], S1[_, _], R1[_, _], PF[_]: Applicative, SF[_]: Applicative](
            argScene: Scene1[IP, PF, P],
            subScene: Scene2[IS, SF, S1],
            augRetell: Retell[Authorization, P, S1, R1, PF, SF]
        ): Exposed[IP, IS, R1] = new:
            def authorize[E, T](ftoken: IP[Token], faction: IS[E, T]): R1[Either[E, AuthError], T] =
                augRetell(
                    argScene(ftoken).map (pt =>
                        subScene(faction).map( fa =>                  
                            [X[e, a]] => (_: Exposed[P, S1, X]).authorize[E, T](pt, fa)
                        )))
          
            def authenticate(fusername: IP[String], fpass: IP[String]): R[AuthError, Token]       = 
                augRetell(
                    argScene(fusername).map2(argScene(fpass)) ((username, pass) =>
                        summon[Applicative[SF]].pure( 
                            [X[e, a]] => (_: Exposed[P, S1, X]).authenticate(username, pass)
                        )))
                
