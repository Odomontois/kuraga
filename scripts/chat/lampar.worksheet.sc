//> using options -Yexplicit-nulls

import scala.collection.mutable.Buffer
import scala.annotation.tailrec
import Value.Value

abstract class Lazy:
  def calc(): Value
  lazy val value = calc()

abstract class Fun:
  def apply(x: Value): Value

object Value:
  opaque type Value >: Int | Fun | Lazy = Int | Fun | Lazy

  extension (v: Value)
    @tailrec final def strict: Value = v match
      case l: Lazy => l.value.strict
      case _       => v

    def toInt: Int           = v.strict.asInstanceOf[Int]
    def app(x: Value): Value = v.strict.asInstanceOf[Fun](x)

  inline def fun(inline f: Value => Value): Value       = (x => f(x)): Fun
  extension (inline expr: => Value) inline def lz: Lazy = () => expr
end Value

enum Lam:
  case Var(idx: Int)
  case App(f: Lam, a: Lam)
  case Abs(l: Lam)

  infix def app(l: Lam) = App(this, l)
  def abs               = Abs(this)

  def eval(stack: Value*): Value = this match
    case Var(idx)  => stack(idx)
    case App(f, a) => f.eval(stack*).app(a.eval(stack*)).lz
    case Abs(l)    => Value.fun(x => l.eval((x +: stack)*)).lz

  def toInt = l"$this 1 0".eval(0, Value.fun(x => x.toInt + 1)).toInt
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
