//> use scala 3.7.3

type Chint  = [A] => A => (A => A) => A
type Chbool = [A] => A => A => A

extension (c: Chbool) def toBool = c(false)(true)
extension (c: Chint) def toInt   = c(0)(_ + 1)

val zero: Chint                      = [A] => z => _ => z
val succ: Chint => Chint             = x => [A] => z => s => s(x(z)(s))
val f: Chbool                        = [A] => f => _ => f
val t: Chbool                        = [A] => _ => t => t
extension (x: Int) def church: Chint = [A] => z => s => Iterator.fill(x)(s).foldRight(z)(_(_))

toInt(succ(succ(zero)))
toBool(f)
toBool(t)
type Pair[A, B] = [C] => (A => B => C) => C
def pair[A, B](x: A, y: B): Pair[A, B] = [C] => f => f(x)(y)
extension [A, B](p: Pair[A, B])
  def fst: A = p(x => _ => x)
  def snd: B = p(_ => x => x)

val let: [A, B] => A => (A => B) => B = [A, B] => x => f => f(x)

val eqZ: Chint => Chbool                         = x => x(t)(_ => f)
val eqSucc: (Chint => Chbool) => Chint => Chbool =
  prev => x => x[Pair[Chbool, Chint]](pair(f, zero))(p => let(p.snd)(u => pair(prev(u), succ(u)))).fst

val eqInt: Chint => Chint => Chbool = x => x(eqZ)(eqSucc)

eqInt(0.church)(0.church).toBool
eqInt(0.church)(2.church).toBool
eqInt(2.church)(0.church).toBool
eqInt(3.church)(3.church).toBool
eqInt(3.church)(5.church).toBool
eqInt(5.church)(3.church).toBool
eqInt(succ(4.church))(5.church).toBool
// toBool(eqInt())


