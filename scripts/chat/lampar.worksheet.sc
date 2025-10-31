//> using options -Yexplicit-nulls

import scala.collection.mutable.Buffer
import scala.annotation.tailrec

trait Lazy(x: () => Value):
  lazy val value = x()

enum Value:
  case Num(x: Int)
  case Func(f: Value => Value)
  case Lz(f: () => Value) extends Value, Lazy(f)

  @tailrec final def strict: Value = this match
    case Lz(t) => t().strict
    case _     => this

  def toInt: Int = this.strict match
    case Num(x) => x
    case _      => throw new Exception(s"Cannot convert $this to Int")

  def app(x: Value): Value = this.strict match
    case Func(f) => f(x)
    case _       => throw new Exception(s"Cannot apply $this to $x")
end Value

object Value:
  extension (inline expr: => Value) inline def lz: Value = Value.Lz(() => expr)
  given Conversion[Int, Value] = Num(_)
  given [A: [t] =>> Conversion[t, Value]]: Conversion[Value => A, Value] = f => Func(f(_))

enum Lam:
  case Var(idx: Int)
  case App(f: Lam, a: Lam)
  case Abs(l: Lam)

  infix def app(l: Lam) = App(this, l)
  def abs               = Abs(this)

  def norm(stack: Value*): Value = this match
    case Var(idx)  => stack(idx)
    case App(f, a) => f.norm(stack*).app(a.norm(stack*)).lz
    case Abs(l)    => Value.Func(x => l.norm((x +: stack)*)).lz

  def toInt = l"$this 1 0".norm(0, (x: Value) => x.toInt + 1).toInt
end Lam

object Lam:
  extension (x: Lam | Null)
    infix inline def ?[R](inline f: Lam => R): R | Null =
      if x == null then null else f(x)
    infix def napp(y: Lam | Null): Lam | Null           =
      if x == null then y
      else if y == null then x
      else x.app(y)

def token = raw"(\(|\)|\$$|\^|\d+)".r

extension (sc: StringContext)
  def l(xs: Lam*): Lam =
    val tokens = token.findAllIn(sc.parts.mkString(" $ "))
    val args   = xs.iterator
    def sub()  = parse(null)

    @tailrec def parse(cur: Lam | Null): Lam | Null =
      tokens.nextOption().getOrElse(")") match
        case "(" => parse(cur napp sub())
        case ")" => cur
        case "$" => parse(cur napp args.next())
        case "^" => parse(cur napp sub() ? (_.abs))
        case num => parse(cur napp Lam.Var(num.toInt))

    sub().nn
end extension

val zero  = l"^^0"
val succ  = l"^^^2 1 (1 0)"
val id    = l"^0"
val plus  = l"^^0 $succ 1"
val mul   = l"^^0 ($plus 1) $zero"
val mul1  = l"^^^^3 (2 1) 0"
val one   = l"$succ $zero"
val pow   = l"^^0 ($mul 1) $one"
val pow1  = l"^^0 1"
val two   = l"$plus $one $one"
val three = l"$plus $two $one"
val six   = l"$mul $two $three"

zero.toInt
one.toInt
six.toInt
l"$mul1 $two $three".toInt
l"$pow1 $two $three".toInt
l"$pow $two $three".toInt
