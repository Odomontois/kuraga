package playground.chat.vec
import scala.compiletime.ops.int.*

enum Nat:
  case S[N <: Nat]()
  case Z()

type %+[N, M <: Nat] <: Nat = N match
  case Z    => M
  case S[n] => S[n %+ M]

import Nat.{S, Z}

enum <=[N <: Nat, M <: Nat]:
  case LZ[N <: Nat]() extends (Z <= N)
  case LS[N <: Nat, M <: Nat](le: N <= M) extends (S[N] <= S[M])
  given [N <: Nat]: (Z <= N)                               = LZ()
  given [N <: Nat, M <: Nat](using N <= M): (S[N] <= S[M]) = LS(summon)

enum Vec[N <: Nat, +A]:
  type Len = N
  case Nil extends Vec[Z, Nothing]
  case %::[N <: Nat, +A](head: A, tail: Vec[N, A]) extends Vec[S[N], A]

  def +:[A1 >: A](h: A1): Vec[S[N], A1] = %::(h, this)

  def ++:[M <: Nat, A1 >: A](v: Vec[M, A1]): Vec[M %+ N, A1] = v match
    case Nil     => this
    case h %:: t =>
      summon[M =:= S[t.Len]].substituteContra[[n] =>> Vec[n %+ N, A1]](h +: t ++: this)

enum Vec1[N <: Int, +A]:
  case Nil extends Vec1[0, Nothing]
  case %::[N <: Int, +A](head: A, tail: Vec1[N, A]) extends Vec1[N + 1, A]

  def apply[i <: Int](i: i)(using i < N =:= true, i >= 0 =:= true): A = Vec1.getUnsafe(this, i)

  def +:[A1 >: A](h: A1): Vec1[N + 1, A1] = %::(h, this)

// def ++:[M <: Int, A1 >: A](v : Vec[M, A1]): Vec[M + N, A1] = v match
//     case Nil => this
//     case h %:: t => h +: t ++: this

object Vec1:
  private def getUnsafe[A](v: Vec1[?, A], i: Int): A = v match
    case Nil     => throw new NoSuchElementException
    case h %:: t => if i == 0 then h else getUnsafe(t, i - 1)

object Vec:
  extension [M <: Nat, N <: Nat, A](v: Vec[S[N], A]) def apply(m: M)(using M <= N): A = get(summon, v)

  def get[M <: Nat, N <: Nat, A](lt: M <= N, v: Vec[S[N], A]): A = (v, lt) match
    case (h %:: _, <=.LZ())  => h
    case (_ %:: t, <=.LS(p)) => get(p, t)
