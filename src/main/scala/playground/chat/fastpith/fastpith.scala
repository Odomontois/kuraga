package playground.chat.fastpith

def coprime(a: Int, b: Int): Boolean =
  if b == 0 then a == 1
  else coprime(b, a % b)

def coprimePiths(max: Long) =
  def fits(u: Int) = u.toLong * u <= max
  for
    b <- Iterator.from(1).takeWhile(fits)
    a <- Iterator.from(b + 1, 2).takeWhile(fits) if coprime(a, b)
    c <- Iterator.from(1, 2).takeWhile(fits)
    a2 = a.toLong * a
    b2 = b.toLong * b
    x  = 2L * a * b * c if x <= max
    y  = (a2 - b2) * c if y <= max
    z  = (a2 + b2) * c if z <= max
  yield (x, y, z)

@main def foo() =
  for (x, y, z) <- coprimePiths(1000) do
    if x * x + y * y != z * z then throw new RuntimeException("ohh")
    else println(s"$x $y $z")
